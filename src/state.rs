use std::f64;

use crate::state::State::{
    ActionState, AfterState, AlwaysSucceedState, FailState, IfState, SelectState, SequenceState,
    WaitForOffState, WaitForOnState, WaitForeverState, WaitState, WhenAllState, WhenAnyState,
    WhileState,
};
use crate::{
    Action, After, AlwaysSucceed, Behavior, Fail, Failure, If, Running, Select, Sequence, Status,
    Success, Wait, WaitForOff, WaitForOn, WaitForever, WhenAll, WhenAny, While,
};

/// The action is still running.
pub const RUNNING: (Status, f64) = (Running, 0.0);

/// The arguments in the action callback.
pub struct ActionArgs<'a, I: 'a, A: 'a, S: 'a> {
    /// The event.
    pub input: &'a Vec<I>,
    /// The remaining delta time.
    pub dt: f64,
    /// The action running.
    pub action: &'a A,
    /// The state of the running action, if any.
    pub state: &'a mut Option<S>,
}

/// Keeps track of a behavior.
#[derive(Clone, Deserialize, Serialize, PartialEq)]
pub enum State<I, A, S> {
    /// Returns `Success` when button is pressed.
    WaitForOnState(I),
    /// Returns `Success` when button is released.
    WaitForOffState(I),
    /// Executes an action.
    ActionState(A, Option<S>),
    /// Converts `Success` into `Failure` and vice versa.
    FailState(Box<State<I, A, S>>),
    /// Ignores failures and always return `Success`.
    AlwaysSucceedState(Box<State<I, A, S>>),
    /// Keeps track of waiting for a period of time before continuing.
    ///
    /// f64: Total time in seconds to wait
    ///
    /// f64: Time elapsed in seconds
    WaitState(f64, f64),
    /// Waits forever.
    WaitForeverState,
    /// Keeps track of an `If` behavior.
    /// If status is `Running`, then it evaluates the condition.
    /// If status is `Success`, then it evaluates the success behavior.
    /// If status is `Failure`, then it evaluates the failure behavior.
    IfState(
        Box<Behavior<I, A>>,
        Box<Behavior<I, A>>,
        Status,
        Box<State<I, A, S>>,
    ),
    /// Keeps track of a `Select` behavior.
    SelectState(Vec<Behavior<I, A>>, usize, Box<State<I, A, S>>),
    /// Keeps track of an `Sequence` behavior.
    SequenceState(Vec<Behavior<I, A>>, usize, Box<State<I, A, S>>),
    /// Keeps track of a `While` behavior.
    WhileState(
        Box<State<I, A, S>>,
        Vec<Behavior<I, A>>,
        usize,
        Box<State<I, A, S>>,
    ),
    /// Keeps track of a `WhenAll` behavior.
    WhenAllState(Vec<Option<State<I, A, S>>>),
    /// Keeps track of a `WhenAny` behavior.
    WhenAnyState(Vec<Option<State<I, A, S>>>),
    /// Keeps track of an `After` behavior.
    AfterState(usize, Vec<State<I, A, S>>),
}

// `Sequence` and `Select` share same algorithm.
//
// `Sequence` fails if any fails and succeeds when all succeeds.
// `Select` succeeds if any succeeds and fails when all fails.
fn sequence<A, S, I, F>(
    select: bool,
    upd: f64,
    seq: &[Behavior<I, A>],
    i: &mut usize,
    cursor: &mut Box<State<I, A, S>>,
    input: &Vec<I>,
    f: &mut F,
) -> (Status, f64)
where
    A: Clone,
    I: Clone + Copy + Sized + PartialEq,
    F: FnMut(ActionArgs<I, A, S>) -> (Status, f64),
{
    let (status, inv_status) = if select {
        // `Select`
        (Failure, Success)
    } else {
        // `Sequence`
        (Success, Failure)
    };
    let mut remaining_dt = upd;
    while *i < seq.len() {
        match cursor.event(remaining_dt, input, f) {
            (Running, _) => {
                break;
            }
            (s, new_dt) if s == inv_status => {
                return (inv_status, new_dt);
            }
            (s, new_dt) if s == status => {
                remaining_dt = new_dt
                //match upd {
                // // Change update event with remaining delta time.
                // Some(_) => new_dt,
                // // Other events are 'consumed' and not passed to next.
                // // If this is the last event, then the sequence succeeded.
                // _ => {
                //     if *i == seq.len() - 1 {
                //         return (status, new_dt);
                //     } else {
                //         *i += 1;
                //         // Create a new cursor for next event.
                //         // Use the same pointer to avoid allocation.
                //         **cursor = State::new(seq[*i].clone());
                //         return RUNNING;
                //     }
                // }
                //}
            }
            _ => unreachable!(),
        };
        *i += 1;
        // If end of sequence,
        // return the 'dt' that is left.
        if *i >= seq.len() {
            return (status, remaining_dt);
        }
        // Create a new cursor for next event.
        // Use the same pointer to avoid allocation.
        **cursor = State::new(seq[*i].clone());
    }
    RUNNING
}

// `WhenAll` and `WhenAny` share same algorithm.
//
// `WhenAll` fails if any fails and succeeds when all succeeds.
// `WhenAny` succeeds if any succeeds and fails when all fails.
fn when_all<A, S, I, F>(
    any: bool,
    dt: f64,
    cursors: &mut Vec<Option<State<I, A, S>>>,
    input: &Vec<I>,
    f: &mut F,
) -> (Status, f64)
where
    A: Clone,
    I: Clone + Copy + Sized + PartialEq,
    F: FnMut(ActionArgs<I, A, S>) -> (Status, f64),
{
    let (status, inv_status) = if any {
        // `WhenAny`
        (Failure, Success)
    } else {
        // `WhenAll`
        (Success, Failure)
    };
    // Get the least delta time left over.
    let mut min_dt = f64::MAX;
    // Count number of terminated events.
    let mut terminated = 0;
    for cur in cursors.iter_mut() {
        match *cur {
            None => {}
            Some(ref mut cur) => {
                match cur.event(dt, input, f) {
                    (Running, _) => {
                        continue;
                    }
                    (s, new_dt) if s == inv_status => {
                        // Fail for `WhenAll`.
                        // Succeed for `WhenAny`.
                        return (inv_status, new_dt);
                    }
                    (s, new_dt) if s == status => {
                        min_dt = min_dt.min(new_dt);
                    }
                    _ => unreachable!(),
                }
            }
        }

        terminated += 1;
        *cur = None;
    }
    match terminated {
        // If there are no events, there is a whole 'dt' left.
        0 if cursors.is_empty() => (status, dt),
        // If all events terminated, the least delta time is left.
        n if cursors.len() == n => (status, min_dt),
        _ => RUNNING,
    }
}

impl<I: Clone + Copy + Sized + PartialEq, A: Clone, S> State<I, A, S> {
    /// Creates a state from a behavior.
    pub fn new(behavior: Behavior<I, A>) -> Self {
        match behavior {
            WaitForOn(button) => WaitForOnState(button),
            WaitForOff(button) => WaitForOffState(button),
            Action(action) => ActionState(action, None),
            Fail(ev) => FailState(Box::new(State::new(*ev))),
            AlwaysSucceed(ev) => AlwaysSucceedState(Box::new(State::new(*ev))),
            Wait(dt) => WaitState(dt, 0.0),
            WaitForever => WaitForeverState,
            If(condition, success, failure) => {
                let state = State::new(*condition);
                IfState(success, failure, Running, Box::new(state))
            }
            Select(sel) => {
                let state = State::new(sel[0].clone());
                SelectState(sel, 0, Box::new(state))
            }
            Sequence(seq) => {
                let state = State::new(seq[0].clone());
                SequenceState(seq, 0, Box::new(state))
            }
            While(ev, rep) => {
                let state = State::new(rep[0].clone());
                WhileState(Box::new(State::new(*ev)), rep, 0, Box::new(state))
            }
            WhenAll(all) => WhenAllState(all.into_iter().map(|ev| Some(State::new(ev))).collect()),
            WhenAny(all) => WhenAnyState(all.into_iter().map(|ev| Some(State::new(ev))).collect()),
            After(seq) => AfterState(0, seq.into_iter().map(State::new).collect()),
        }
    }

    /// Updates the cursor that tracks an event.
    ///
    /// The action need to return status and remaining delta time.
    /// Returns status and the remaining delta time.
    ///
    /// Passes event, delta time in seconds, action and state to closure.
    /// The closure should return a status and remaining delta time.
    pub fn event<F>(&mut self, dt: f64, input: &Vec<I>, f: &mut F) -> (Status, f64)
    where
        F: FnMut(ActionArgs<I, A, S>) -> (Status, f64),
    {
        //let upd = dt;
        match self {
            &mut WaitForOnState(button) => {
                if input.contains(&button) {
                    (Success, dt)
                } else {
                    RUNNING
                }
            }
            &mut WaitForOffState(button) => {
                if !input.contains(&button) {
                    (Success, dt)
                } else {
                    RUNNING
                }
            }
            &mut ActionState(ref action, ref mut state) => {
                // Execute action.
                f(ActionArgs {
                    input,
                    dt,
                    action,
                    state,
                })
            }
            &mut FailState(ref mut cur) => match cur.event(dt, input, f) {
                (Running, dt) => (Running, dt),
                (Failure, dt) => (Success, dt),
                (Success, dt) => (Failure, dt),
            },
            &mut AlwaysSucceedState(ref mut cur) => match cur.event(dt, input, f) {
                (Running, dt) => (Running, dt),
                (_, dt) => (Success, dt),
            },
            &mut WaitState(wait_t, ref mut t) => {
                if *t + dt >= wait_t {
                    let remaining_dt = *t + dt - wait_t;
                    *t = wait_t;
                    (Success, remaining_dt)
                } else {
                    *t += dt;
                    RUNNING
                }
            }
            &mut IfState(ref success, ref failure, ref mut status, ref mut state) => {
                let mut remaining_dt = dt;
                // Run in a loop to evaluate success or failure with
                // remaining delta time after condition.
                loop {
                    *status = match *status {
                        Running => match state.event(remaining_dt, input, f) {
                            (Running, dt) => {
                                return (Running, dt);
                            }
                            (Success, dt) => {
                                **state = State::new((**success).clone());
                                remaining_dt = dt;
                                Success
                            }
                            (Failure, dt) => {
                                **state = State::new((**failure).clone());
                                remaining_dt = dt;
                                Failure
                            }
                        },
                        _ => {
                            return state.event(dt, input, f);
                        }
                    }
                }
            }
            &mut SelectState(ref seq, ref mut i, ref mut cursor) => {
                let select = true;
                sequence(select, dt, seq, i, cursor, input, f)
            }
            &mut SequenceState(ref seq, ref mut i, ref mut cursor) => {
                let select = false;
                sequence(select, dt, seq, i, cursor, input, f)
            }
            &mut WhileState(ref mut ev_cursor, ref rep, ref mut i, ref mut cursor) => {
                // If the event terminates, do not execute the loop.
                match ev_cursor.event(dt, input, f) {
                    (Running, _) => {}
                    x => return x,
                };
                let cur = cursor;
                let mut remaining_dt = dt;
                loop {
                    match cur.event(remaining_dt, input, f) {
                        (Failure, x) => return (Failure, x),
                        (Running, _) => break,
                        (Success, new_dt) => remaining_dt = new_dt,
                    };
                    *i += 1;
                    // If end of repeated events,
                    // start over from the first one.
                    if *i >= rep.len() {
                        *i = 0;
                    }
                    // Create a new cursor for next event.
                    // Use the same pointer to avoid allocation.
                    **cur = State::new(rep[*i].clone());
                }
                RUNNING
            }
            &mut WhenAllState(ref mut cursors) => {
                let any = false;
                when_all(any, dt, cursors, input, f)
            }
            &mut WhenAnyState(ref mut cursors) => {
                let any = true;
                when_all(any, dt, cursors, input, f)
            }
            &mut AfterState(ref mut i, ref mut cursors) => {
                // Get the least delta time left over.
                let mut min_dt = f64::MAX;
                for j in *i..cursors.len() {
                    match cursors[j].event(dt, input, f) {
                        (Running, _) => {
                            min_dt = 0.0;
                        }
                        (Success, new_dt) => {
                            // Remaining delta time must be less to succeed.
                            if *i == j && new_dt < min_dt {
                                *i += 1;
                                min_dt = new_dt;
                            } else {
                                // Return least delta time because
                                // that is when failure is detected.
                                return (Failure, min_dt.min(new_dt));
                            }
                        }
                        (Failure, new_dt) => {
                            return (Failure, new_dt);
                        }
                    };
                }
                if *i == cursors.len() {
                    (Success, min_dt)
                } else {
                    RUNNING
                }
            }
            _ => RUNNING,
        }
    }
}
