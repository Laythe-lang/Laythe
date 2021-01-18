use std::ops::Range;

use codespan_reporting::files;
use laythe_core::object::Map;
use laythe_env::managed::{GcStr, Trace};

/// A set of offsets indicating the
#[derive(Default, Clone)]
pub struct LineOffsets {
  offsets: Vec<usize>,
  len: usize,
}

impl LineOffsets {
  pub fn new(offsets: Vec<usize>, len: usize) -> Self {
    assert!(!offsets.is_empty());
    assert!(*offsets.last().unwrap_or(&0) <= len);

    Self { offsets, len }
  }

  /// Retrieve the total number of lines cataloged
  ///
  /// # Examples
  /// ```
  /// use laythe_vm::files::LineOffsets;
  ///
  /// let offsets = LineOffsets::new(vec![0, 10], 20);
  /// assert_eq!(offsets.lines(), 2);
  /// ```
  pub fn lines(&self) -> usize {
    self.offsets.len()
  }

  /// What line is the provided offset located at
  ///
  /// # Examples
  /// ```
  /// use laythe_vm::files::LineOffsets;
  ///
  /// let offsets = LineOffsets::new(vec![0, 10], 20);
  /// assert_eq!(offsets.offset_line(0), Ok(0));
  /// assert_eq!(offsets.offset_line(5), Ok(0));
  /// assert_eq!(offsets.offset_line(11), Ok(1));
  /// assert_eq!(offsets.offset_line(17), Ok(1));
  /// assert_eq!(offsets.offset_line(20), Ok(1));
  /// assert_eq!(offsets.offset_line(25), Err(()));
  /// ```
  pub fn offset_line(&self, offset: usize) -> Result<usize, ()> {
    if offset > self.len {
      return Err(());
    }

    Ok(match self.offsets.binary_search(&offset) {
      Ok(line) => line,
      Err(line) => line - 1,
    })
  }

  /// Get the offset ranges bounding a line
  ///
  /// # Examples
  /// ```
  /// use laythe_vm::files::LineOffsets;
  ///
  /// let offsets = LineOffsets::new(vec![0, 10], 20);
  /// assert_eq!(offsets.line_range(0), Ok(0..10));
  /// assert_eq!(offsets.line_range(1), Ok(10..20));
  /// assert_eq!(offsets.line_range(2), Err(()));
  /// ```
  pub fn line_range(&self, line: usize) -> Result<Range<usize>, ()> {
    let lines = self.lines();
    if line >= lines {
      return Err(());
    }

    if line == (self.lines()) - 1 {
      Ok(self.offsets[line]..self.len)
    } else {
      Ok(self.offsets[line]..self.offsets[(line) + 1])
    }
  }
}

struct VmFile {
  name: GcStr,
  source: GcStr,
  line_offsets: Option<LineOffsets>,
}

impl VmFile {
  fn line_offsets(&self) -> &LineOffsets {
    self.line_offsets.as_ref().expect("Line offset not set yet")
  }
}

impl Trace for VmFile {
  fn trace(&self) {
    self.name.trace();
    self.source.trace();
  }

  fn trace_debug(&self, log: &mut dyn std::io::Write) {
    self.name.trace_debug(log);
    self.source.trace_debug(log);
  }
}

#[derive(Clone, Copy, PartialEq)]
pub struct VmFileId(usize);

#[derive(Default)]
pub struct VmFiles {
  files: Vec<VmFile>,
  name_map: Map<GcStr, usize>,
}

impl VmFiles {
  fn get<'a>(&'a self, id: VmFileId) -> Result<&'a VmFile, files::Error> {
    if id.0 > self.files.len() {
      return Err(files::Error::FileMissing);
    }

    Ok(&self.files[id.0])
  }

  fn get_mut<'a>(&'a mut self, id: VmFileId) -> Result<&'a mut VmFile, files::Error> {
    if id.0 > self.files.len() {
      return Err(files::Error::FileMissing);
    }

    Ok(&mut self.files[id.0])
  }

  pub fn upsert(&mut self, name: GcStr, source: GcStr) -> VmFileId {
    match self.name_map.get(&name) {
      Some(id) => {
        self.files[*id] = VmFile {
          name,
          source,
          line_offsets: None,
        };

        VmFileId(*id)
      }
      None => {
        self.files.push(VmFile {
          name,
          source,
          line_offsets: None,
        });

        let id = self.files.len() - 1;
        self.name_map.insert(name, id);
        VmFileId(id)
      }
    }
  }

  pub fn update_line_offsets(
    &mut self,
    id: VmFileId,
    line_offsets: LineOffsets,
  ) -> Result<(), files::Error> {
    let vm_file = self.get_mut(id)?;
    vm_file.line_offsets = Some(line_offsets);
    Ok(())
  }
}

impl<'a> files::Files<'a> for VmFiles {
  type FileId = VmFileId;
  type Name = GcStr;
  type Source = GcStr;

  fn name(&'a self, id: Self::FileId) -> Result<Self::Name, files::Error> {
    Ok(self.get(id)?.name)
  }

  fn source(&'a self, id: Self::FileId) -> Result<Self::Source, files::Error> {
    Ok(self.get(id)?.source)
  }

  fn line_index(&'a self, id: Self::FileId, byte_index: usize) -> Result<usize, files::Error> {
    let vm_file = self.get(id)?;

    match vm_file.line_offsets().offset_line(byte_index) {
      Ok(line) => Ok(line),
      Err(_) => {
        return Err(files::Error::IndexTooLarge {
          given: byte_index,
          max: vm_file.source.len(),
        })
      }
    }
  }

  fn line_range(
    &'a self,
    id: Self::FileId,
    line_index: usize,
  ) -> Result<std::ops::Range<usize>, files::Error> {
    let vm_file = self.get(id)?;
    let line_offset = vm_file.line_offsets();

    match line_offset.line_range(line_index) {
      Ok(range) => Ok((range.start as usize)..(range.end as usize)),
      Err(_) => Err(files::Error::LineTooLarge {
        given: line_index,
        max: vm_file.source.len(),
      }),
    }
  }
}

impl Trace for VmFiles {
  fn trace(&self) {
    self.files.iter().for_each(|file| file.trace());
  }

  fn trace_debug(&self, log: &mut dyn std::io::Write) {
    self.files.iter().for_each(|file| file.trace_debug(log));
  }
}
