use std::ops::Range;

use codespan_reporting::files;
use laythe_core::{
  managed::{GcStr, Trace},
  object::Map,
};

/// A struct for efficiently determine lines for an associated
/// file.
#[derive(Default, Clone)]
pub struct LineOffsets {
  /// The offsets where line break occur in the associated file
  offsets: Vec<usize>,

  /// The full length of the file
  len: usize,
}

#[derive(Debug, PartialEq)]
pub enum LineError {
  OffsetOutOfBounds,
  LineOutOfBounds,
}

impl LineOffsets {
  /// Create a new instance of LineOffsets
  ///
  /// # Examples
  /// ```
  /// use laythe_vm::source::LineOffsets;
  ///
  /// let offsets = LineOffsets::new(vec![0, 10], 20);
  /// ```
  pub fn new(offsets: Vec<usize>, len: usize) -> Self {
    assert!(!offsets.is_empty());
    assert!(*offsets.last().unwrap_or(&0) <= len);

    Self { offsets, len }
  }

  /// Retrieve the total number of lines cataloged
  ///
  /// # Examples
  /// ```
  /// use laythe_vm::source::LineOffsets;
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
  /// use laythe_vm::source::{LineOffsets, LineError};
  ///
  /// let offsets = LineOffsets::new(vec![0, 10], 20);
  /// assert_eq!(offsets.offset_line(0), Ok(0));
  /// assert_eq!(offsets.offset_line(5), Ok(0));
  /// assert_eq!(offsets.offset_line(11), Ok(1));
  /// assert_eq!(offsets.offset_line(17), Ok(1));
  /// assert_eq!(offsets.offset_line(20), Ok(1));
  /// assert_eq!(offsets.offset_line(25), Err(LineError::OffsetOutOfBounds));
  /// ```
  pub fn offset_line(&self, offset: usize) -> Result<usize, LineError> {
    if offset > self.len {
      return Err(LineError::OffsetOutOfBounds);
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
  /// use laythe_vm::source::{LineOffsets, LineError};
  ///
  /// let offsets = LineOffsets::new(vec![0, 10], 20);
  /// assert_eq!(offsets.line_range(0), Ok(0..10));
  /// assert_eq!(offsets.line_range(1), Ok(10..20));
  /// assert_eq!(offsets.line_range(2), Err(LineError::LineOutOfBounds));
  /// ```
  pub fn line_range(&self, line: usize) -> Result<Range<usize>, LineError> {
    let lines = self.lines();
    if line >= lines {
      return Err(LineError::LineOutOfBounds);
    }

    if line == (self.lines()) - 1 {
      Ok(self.offsets[line]..self.len)
    } else {
      Ok(self.offsets[line]..self.offsets[(line) + 1])
    }
  }
}

/// A struct for holding information related to file loaded
/// by the virtual machine
struct VmFile {
  /// The name of this file
  name: GcStr,

  /// The full source of this file
  source: GcStr,

  /// The line offsets for this file
  line_offsets: Option<LineOffsets>,
}

impl VmFile {
  /// Get an immutable reference to files line offsets
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

/// A unique id to a `VmFile`
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct VmFileId(usize);

/// A file database managed by the virtual machine
#[derive(Default)]
pub struct VmFiles {
  /// The files stored in this file database
  files: Vec<VmFile>,

  /// A map between file names and there unwrapped
  name_map: Map<GcStr, usize>,
}

impl VmFiles {
  /// Insert or update a file into the file database. Returns the
  /// `VmFileId` for the new file or updated file.
  ///
  /// # Examples
  /// ```
  /// use laythe_core::memory::{Allocator, NO_GC};
  /// use laythe_vm::source::VmFiles;
  ///
  /// let mut alloc = Allocator::default();
  /// let name1 = alloc.manage_str("first.lay", &NO_GC);
  /// let name2 = alloc.manage_str("second.lay", &NO_GC);
  ///
  /// let source1 = alloc.manage_str("let x = 10;", &NO_GC);
  /// let source2 = alloc.manage_str("print(\"hi\")", &NO_GC);
  ///
  /// let mut files = VmFiles::default();
  /// let id1 = files.upsert(name1, source1);
  /// let id2 = files.upsert(name2, source1);
  /// let id3 = files.upsert(name1, source2);
  ///
  /// assert_ne!(id1, id2);
  /// assert_ne!(id2, id3);
  /// assert_eq!(id1, id3);
  /// ```
  pub fn upsert(&mut self, name: GcStr, source: GcStr) -> VmFileId {
    match self.name_map.get(&name) {
      Some(id) => {
        self.files[*id] = VmFile {
          name,
          source,
          line_offsets: None,
        };

        VmFileId(*id)
      },
      None => {
        self.files.push(VmFile {
          name,
          source,
          line_offsets: None,
        });

        let id = self.files.len() - 1;
        self.name_map.insert(name, id);
        VmFileId(id)
      },
    }
  }

  /// Update a files line offsets after it has been calculated
  ///
  /// # Examples
  /// ```
  /// use laythe_core::memory::{Allocator, NO_GC};
  /// use laythe_vm::source::{VmFiles, LineOffsets};
  ///
  /// let mut alloc = Allocator::default();
  ///
  /// let name = alloc.manage_str("first.lay", &NO_GC);
  /// let source = alloc.manage_str("let x = 10;", &NO_GC);
  ///
  /// let mut files = VmFiles::default();
  /// let id = files.upsert(name, source);
  ///
  /// assert!(files.update_line_offsets(id, LineOffsets::new(vec![0], 10)).is_ok());
  /// ```
  pub fn update_line_offsets(
    &mut self,
    id: VmFileId,
    line_offsets: LineOffsets,
  ) -> Result<(), files::Error> {
    let vm_file = self.get_mut(id)?;
    vm_file.line_offsets = Some(line_offsets);
    Ok(())
  }

  /// Retrieve an immutable file reference from the file database. Return
  /// a file missing error if not found
  fn get(&'_ self, id: VmFileId) -> Result<&'_ VmFile, files::Error> {
    if id.0 > self.files.len() {
      return Err(files::Error::FileMissing);
    }

    Ok(&self.files[id.0])
  }

  /// Retrieve an mutable file reference from the file database. Return
  /// a file missing error if not found
  fn get_mut(&'_ mut self, id: VmFileId) -> Result<&'_ mut VmFile, files::Error> {
    if id.0 > self.files.len() {
      return Err(files::Error::FileMissing);
    }

    Ok(&mut self.files[id.0])
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
      Err(_) => Err(files::Error::IndexTooLarge {
        given: byte_index,
        max: vm_file.source.len(),
      }),
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

#[cfg(test)]
mod test {
  use super::*;
  mod line_offsets {
    use super::*;

    #[test]
    fn lines() {
      let offsets = LineOffsets::new(vec![0, 10], 20);
      assert_eq!(offsets.lines(), 2);
    }

    #[test]
    fn offset_line() {
      let offsets = LineOffsets::new(vec![0, 10], 20);

      assert_eq!(offsets.offset_line(0), Ok(0));
      assert_eq!(offsets.offset_line(1), Ok(0));
      assert_eq!(offsets.offset_line(10), Ok(1));
      assert_eq!(offsets.offset_line(11), Ok(1));
      assert_eq!(offsets.offset_line(20), Ok(1));
      assert_eq!(offsets.offset_line(30), Err(LineError::OffsetOutOfBounds));
    }

    #[test]
    fn line_range() {
      let offsets = LineOffsets::new(vec![0, 10], 20);

      assert_eq!(offsets.line_range(0), Ok(0..10));
      assert_eq!(offsets.line_range(1), Ok(10..20));
      assert_eq!(offsets.line_range(2), Err(LineError::LineOutOfBounds));
    }
  }

  mod vm_files {}
}
