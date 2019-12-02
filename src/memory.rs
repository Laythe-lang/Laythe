use crate::object::Obj;

pub fn free_objects(head: &Obj) {
  let mut obj = head;
  loop {
    let next = &obj.next;
    free_object(obj);

    match next.get() {
      Some(next_obj) => {
        obj = next_obj;
      }
      None => {
        return;
      }
    }
  }
}

fn free_object(_obj: &Obj) {}
