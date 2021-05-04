use crossbeam_channel::{bounded, unbounded, Receiver, Sender};
use std::thread;

#[derive(Debug)]
pub struct Thread {
    handle: thread::JoinHandle<()>,
    sender: Sender<Box<dyn FnOnce() + Send + 'static>>,
}

impl Thread {
    pub fn spawn() -> Self {
        let (sender, receiver) = unbounded();
        let handle = thread::spawn(move || process_tasks(receiver));
        Self { handle, sender }
    }

    pub fn run<F, T>(&self, f: F) -> JoinHandle<T>
    where
        F: FnOnce() -> T,
        F: Send + 'static,
        T: Send + 'static,
    {
        let (sender, receiver) = bounded(0);
        self.sender
            .send(Box::new(move || {
                let _ = sender.send(f());
            }))
            .unwrap();
        JoinHandle { receiver }
    }
}

#[derive(Debug)]
pub struct JoinHandle<T> {
    receiver: Receiver<T>,
}

impl<T> JoinHandle<T> {
    pub fn join(self) -> T {
        self.receiver.recv().unwrap()
    }
}

pub fn process_tasks(receiver: Receiver<Box<dyn FnOnce() + Send + 'static>>) {
    while let Ok(f) = receiver.recv() {
        f();
    }
}
