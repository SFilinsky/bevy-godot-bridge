//! Wake a publisher once when values arrive, without checking every store each frame.
use std::cell::{Cell, RefCell};
use std::mem::take;
use std::rc::{Rc, Weak};
observe! { use std::time::Instant; }

/// Keep this handle with the producer; dropping it cancels pending publication.
pub struct ExportPublication {
    pending: Cell<bool>,
    queue: Weak<RefCell<Vec<Weak<ExportPublication>>>>,
    publish: Box<dyn Fn()>,
    #[cfg(any(
        all(debug_assertions, feature = "dev-observability"),
        feature = "profiled-release"
    ))]
    queued_at: Cell<Option<Instant>>,
}

impl ExportPublication {
    /// Request publication once, even if many entities changed in the same batch.
    pub fn request(self: &Rc<Self>) {
        let Some(queue) = self.queue.upgrade() else {
            return;
        };
        if !self.pending.replace(true) {
            observe! { self.queued_at.set(Some(Instant::now())); }
            queue.borrow_mut().push(Rc::downgrade(self));
        }
    }
}

#[derive(Clone, Default)]
pub(crate) struct PublicationQueue(Rc<RefCell<Vec<Weak<ExportPublication>>>>);

impl PublicationQueue {
    pub(crate) fn register(&self, publish: impl Fn() + 'static) -> Rc<ExportPublication> {
        let registration = Rc::new(ExportPublication {
            pending: Cell::new(false),
            queue: Rc::downgrade(&self.0),
            publish: Box::new(publish),
            #[cfg(any(
                all(debug_assertions, feature = "dev-observability"),
                feature = "profiled-release"
            ))]
            queued_at: Cell::new(None),
        });
        // Include values queued before the authored node finished entering the tree.
        registration.request();
        registration
    }

    pub(crate) fn pending(&self) -> bool {
        !self.0.borrow().is_empty()
    }
    pub(crate) fn clear(&self) {
        self.0.borrow_mut().clear();
    }

    #[cfg(any(
        all(debug_assertions, feature = "dev-observability"),
        feature = "profiled-release"
    ))]
    pub(crate) fn longest_wait_usec(&self) -> u64 {
        self.0
            .borrow()
            .iter()
            .filter_map(Weak::upgrade)
            .filter_map(|registration| registration.queued_at.get())
            .map(|queued_at| queued_at.elapsed().as_micros() as u64)
            .max()
            .unwrap_or(0)
    }

    pub(crate) fn deliver(&self, keep_running: impl Fn() -> bool) {
        // Release the queue before callbacks; a callback may request more work.
        let mut work = take(&mut *self.0.borrow_mut());
        for weak in work.drain(..) {
            if !keep_running() {
                break;
            }
            if let Some(registration) = weak.upgrade() {
                registration.pending.set(false);
                (registration.publish)();
            }
        }
        // Keep any newly queued work for the next phase and reuse the allocation.
        work.append(&mut *self.0.borrow_mut());
        *self.0.borrow_mut() = work;
    }
}

#[cfg(test)]
mod tests {
    use super::PublicationQueue;
    use std::cell::Cell;
    use std::rc::Rc;

    #[test]
    fn only_requested_publishers_run_and_repeated_requests_merge() {
        let queue = PublicationQueue::default();
        let count = Rc::new(Cell::new(0));
        let captured = count.clone();
        let registration = queue.register(move || captured.set(captured.get() + 1));
        queue.deliver(|| true);
        queue.deliver(|| true);
        assert_eq!(count.get(), 1);
        assert!(!queue.pending());
        registration.request();
        registration.request();
        queue.deliver(|| true);
        assert_eq!(count.get(), 2);
    }

    #[test]
    fn dropping_the_producer_cancels_its_pending_callback() {
        let queue = PublicationQueue::default();
        let registration = queue.register(|| panic!("cancelled publisher ran"));
        drop(registration);
        queue.deliver(|| true);
        assert!(!queue.pending());
    }
}
