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
pub const fn RUNNING<I>(input: Option<Vec<I>>) -> (Status, f64, Option<Vec<I>>) {
    (Running, 0.0, input)
}

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
) -> (Status, f64, Option<Vec<I>>)
where
    A: Clone,
    I: Clone + Copy + Sized + PartialEq,
    F: FnMut(ActionArgs<I, A, S>) -> (Status, f64, Option<Vec<I>>),
{
    let mut upd_input: Option<Vec<I>> = None;

    let (status, inv_status) = if select {
        // `Select`
        (Failure, Success)
    } else {
        // `Sequence`
        (Success, Failure)
    };
    let mut remaining_dt = upd;
    while *i < seq.len() {
        match cursor.event(remaining_dt, upd_input.as_ref().unwrap_or(input), f) {
            (Running, _, Some(new_input)) => {
                upd_input = Some(new_input);
                break;
            }
            (Running, _, _) => {
                break;
            }
            (s, new_dt, input) if s == inv_status => {
                return (inv_status, new_dt, input);
            }
            (s, new_dt, Some(new_input)) if s == status => {
                upd_input = Some(new_input);
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
            (s, new_dt, _) if s == status => {
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
            return (status, remaining_dt, upd_input);
        }
        // Create a new cursor for next event.
        // Use the same pointer to avoid allocation.
        **cursor = State::new(seq[*i].clone());
    }
    RUNNING(upd_input)
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
) -> (Status, f64, Option<Vec<I>>)
where
    A: Clone,
    I: Clone + Copy + Sized + PartialEq,
    F: FnMut(ActionArgs<I, A, S>) -> (Status, f64, Option<Vec<I>>),
{
    let mut upd_input: Option<Vec<I>> = None;

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
                match cur.event(dt, upd_input.as_ref().unwrap_or(input), f) {
                    (Running, _, _) => {
                        continue;
                    }
                    (s, new_dt, Some(new_input)) if s == inv_status => {
                        // Fail for `WhenAll`.
                        // Succeed for `WhenAny`.
                        return (inv_status, new_dt, Some(new_input));
                    }
                    (s, new_dt, _) if s == inv_status => {
                        // Fail for `WhenAll`.
                        // Succeed for `WhenAny`.
                        return (inv_status, new_dt, upd_input);
                    }
                    (s, new_dt, Some(new_input)) if s == status => {
                        min_dt = min_dt.min(new_dt);
                        upd_input = Some(new_input);
                    }
                    (s, new_dt, _) if s == status => {
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
        0 if cursors.is_empty() => (status, dt, upd_input),
        // If all events terminated, the least delta time is left.
        n if cursors.len() == n => (status, min_dt, upd_input),
        _ => RUNNING(upd_input),
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
    pub fn event<F>(&mut self, dt: f64, input: &Vec<I>, f: &mut F) -> (Status, f64, Option<Vec<I>>)
    where
        F: FnMut(ActionArgs<I, A, S>) -> (Status, f64, Option<Vec<I>>),
    {
        let mut upd_input: Option<Vec<I>> = None;

        match self {
            &mut WaitForOnState(button) => {
                if input.contains(&button) {
                    (Success, dt, None)
                } else {
                    RUNNING(upd_input)
                }
            }
            &mut WaitForOffState(button) => {
                if !input.contains(&button) {
                    (Success, dt, None)
                } else {
                    RUNNING(upd_input)
                }
            }
            &mut ActionState(ref action, ref mut state) => {
                // Execute action.
                f(ActionArgs {
                    input: &input,
                    dt,
                    action,
                    state,
                })
            }
            &mut FailState(ref mut cur) => {
                match cur.event(dt, upd_input.as_ref().unwrap_or(input), f) {
                    (Running, dt, input) => (Running, dt, input),
                    (Failure, dt, input) => (Success, dt, input),
                    (Success, dt, input) => (Failure, dt, input),
                }
            }
            &mut AlwaysSucceedState(ref mut cur) => {
                match cur.event(dt, upd_input.as_ref().unwrap_or(input), f) {
                    (Running, dt, input) => (Running, dt, input),
                    (_, dt, input) => (Success, dt, input),
                }
            }
            &mut WaitState(wait_t, ref mut t) => {
                if *t + dt >= wait_t {
                    let remaining_dt = *t + dt - wait_t;
                    *t = wait_t;
                    (Success, remaining_dt, None)
                } else {
                    *t += dt;
                    RUNNING(upd_input)
                }
            }
            &mut IfState(ref success, ref failure, ref mut status, ref mut state) => {
                let mut remaining_dt = dt;
                // Run in a loop to evaluate success or failure with
                // remaining delta time after condition.
                loop {
                    *status = match *status {
                        Running => {
                            match state.event(remaining_dt, upd_input.as_ref().unwrap_or(input), f)
                            {
                                (Running, dt, input) => {
                                    return (Running, dt, input);
                                }
                                (Success, dt, Some(new_input)) => {
                                    **state = State::new((**success).clone());
                                    upd_input = Some(new_input);
                                    remaining_dt = dt;
                                    Success
                                }
                                (Success, dt, _) => {
                                    **state = State::new((**success).clone());
                                    remaining_dt = dt;
                                    Success
                                }
                                (Failure, dt, Some(new_input)) => {
                                    **state = State::new((**failure).clone());
                                    upd_input = Some(new_input);
                                    remaining_dt = dt;
                                    Failure
                                }
                                (Failure, dt, _) => {
                                    **state = State::new((**failure).clone());
                                    remaining_dt = dt;
                                    Failure
                                }
                            }
                        }
                        _ => {
                            return state.event(dt, upd_input.as_ref().unwrap_or(input), f);
                        }
                    }
                }
            }
            &mut SelectState(ref seq, ref mut i, ref mut cursor) => {
                let select = true;
                sequence(
                    select,
                    dt,
                    seq,
                    i,
                    cursor,
                    upd_input.as_ref().unwrap_or(input),
                    f,
                )
            }
            &mut SequenceState(ref seq, ref mut i, ref mut cursor) => {
                let select = false;
                sequence(
                    select,
                    dt,
                    seq,
                    i,
                    cursor,
                    upd_input.as_ref().unwrap_or(input),
                    f,
                )
            }
            &mut WhileState(ref mut ev_cursor, ref rep, ref mut i, ref mut cursor) => {
                // If the event terminates, do not execute the loop.
                match ev_cursor.event(dt, upd_input.as_ref().unwrap_or(input), f) {
                    (Running, _, _) => {}
                    x => return x,
                };
                let cur = cursor;
                let mut remaining_dt = dt;
                loop {
                    match cur.event(remaining_dt, upd_input.as_ref().unwrap_or(input), f) {
                        (Failure, x, input) => return (Failure, x, input),
                        (Running, _, _) => break,
                        (Success, new_dt, Some(new_input)) => {
                            upd_input = Some(new_input);
                            remaining_dt = new_dt
                        }
                        (Success, new_dt, _) => remaining_dt = new_dt,
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
                RUNNING(upd_input)
            }
            &mut WhenAllState(ref mut cursors) => {
                let any = false;
                when_all(any, dt, cursors, &input, f)
            }
            &mut WhenAnyState(ref mut cursors) => {
                let any = true;
                when_all(any, dt, cursors, &input, f)
            }
            &mut AfterState(ref mut i, ref mut cursors) => {
                // Get the least delta time left over.
                let mut min_dt = f64::MAX;
                for j in *i..cursors.len() {
                    match cursors[j].event(dt, upd_input.as_ref().unwrap_or(input), f) {
                        (Running, _, _) => {
                            min_dt = 0.0;
                        }
                        (Success, new_dt, Some(new_input)) => {
                            // Remaining delta time must be less to succeed.
                            if *i == j && new_dt < min_dt {
                                *i += 1;
                                min_dt = new_dt;
                                upd_input = Some(new_input);
                            } else {
                                // Return least delta time because
                                // that is when failure is detected.
                                return (Failure, min_dt.min(new_dt), Some(new_input));
                            }
                        }
                        (Success, new_dt, _) => {
                            // Remaining delta time must be less to succeed.
                            if *i == j && new_dt < min_dt {
                                *i += 1;
                                min_dt = new_dt;
                            } else {
                                // Return least delta time because
                                // that is when failure is detected.
                                return (Failure, min_dt.min(new_dt), None);
                            }
                        }
                        (Failure, new_dt, new_input) => {
                            return (Failure, new_dt, new_input);
                        }
                    };
                }
                if *i == cursors.len() {
                    (Success, min_dt, None)
                } else {
                    RUNNING(upd_input)
                }
            }
            _ => RUNNING(upd_input),
        }
    }
}
