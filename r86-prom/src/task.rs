use indicatif::{ProgressBar, ProgressStyle};

use crate::programmer::Programmer;

#[derive(Copy, Clone)]
pub enum TaskKind {
    Spinner,
    Progress(u64)
}

pub struct TaskSchedule<'a, T = ()> {
    state: T,
    tasks: Vec<(TaskKind, &'a str, Box<dyn Fn (&mut Programmer, &ProgressBar, &mut T)>)>,
    spinner_style: ProgressStyle,
    bar_style: ProgressStyle
}

impl<'a, T> TaskSchedule<'a, T>
where
    T: Default
{
    pub fn new() -> Self {
        TaskSchedule {
            state: Default::default(),
            tasks: Vec::new(),
            spinner_style: ProgressStyle::default_spinner()
                .template("{prefix:.bold.dim} {spinner} {wide_msg}"),
            bar_style: ProgressStyle::default_bar()
                .template("{prefix:.bold.dim} {spinner} {msg} {bar:64.cyan/blue}  {percent}%")
                .progress_chars("#>-")
        }
    }
}

impl<'a, T> TaskSchedule<'a, T> {
    pub fn with_state(state: T) -> Self {
        TaskSchedule {
            state,
            tasks: Vec::new(),
            spinner_style: ProgressStyle::default_spinner()
                .template("{prefix:.bold.dim} {spinner} {wide_msg}"),
            bar_style: ProgressStyle::default_bar()
                .template("{prefix:.bold.dim} {spinner} {msg} {bar:64.cyan/blue}  {percent}%")
                .progress_chars("#>-")
        }
    }

    pub fn set_spinner_style(&mut self, style: ProgressStyle) {
        self.spinner_style = style;
    }

    pub fn set_bar_style(&mut self, style: ProgressStyle) {
        self.bar_style = style;
    }

    pub fn schedule<F>(&mut self, kind: TaskKind, message: &'a str, task: F)
        where F: 'static + Fn (&mut Programmer, &ProgressBar, &mut T) {
        self.tasks.push((kind, message, Box::new(task)))
    }

    pub fn schedule_fast<F>(&mut self, message: &'a str, task: F)
        where F: 'static + Fn (&mut Programmer, &ProgressBar, &mut T) {
        self.tasks.push((TaskKind::Spinner, message, Box::new(task)))
    }

    pub fn run<F>(&mut self, initiator_kind: TaskKind, initiator_message: &'a str, initiator: F)
        where F: 'static + Fn(&ProgressBar, &mut T) -> Programmer {

        let progress = match initiator_kind {
            TaskKind::Spinner => {
                let progress = ProgressBar::new_spinner();
                progress.set_style(self.spinner_style.clone());
                progress.set_prefix(&format!("[ 1/{:2}]", self.tasks.len() + 1));
                progress
            },
            TaskKind::Progress(amount) => {
                let progress = ProgressBar::new(amount);
                progress.set_style(self.bar_style.clone());
                progress.set_prefix(&format!("[ 1/{:2}]", self.tasks.len() + 1));
                progress
            }
        };
        progress.set_message(initiator_message);

        let mut programmer = initiator(&progress, &mut self.state);

        progress.finish();

        for ((kind, message, task), index) in self.tasks.iter().zip(0..self.tasks.len()) {
            let progress = match kind {
                TaskKind::Spinner => {
                    let progress = ProgressBar::new_spinner();
                    progress.set_style(self.spinner_style.clone());
                    progress.set_prefix(&format!("[{:2}/{:2}]", index + 2, self.tasks.len() + 1));
                    progress
                },
                TaskKind::Progress(amount) => {
                    let progress = ProgressBar::new(*amount);
                    progress.set_style(self.bar_style.clone());
                    progress.set_prefix(&format!("[{:2}/{:2}]", index + 2, self.tasks.len() + 1));
                    progress
                }
            };
            progress.set_message(message);

            task(&mut programmer, &progress, &mut self.state);

            if !progress.is_finished() { progress.finish(); }
        }
    }

    pub fn state(&self) -> &T {
        &self.state
    }

    pub fn state_mut(&mut self) -> &mut T {
        &mut self.state
    }

    pub fn into_state(self) -> T {
        self.state
    }
}