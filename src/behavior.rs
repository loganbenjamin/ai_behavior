/// Describes a behavior.
///
/// This is used for more complex event logic.
/// Can also be used for game AI.
#[derive(Clone, Deserialize, Serialize, PartialEq)]
pub enum Behavior<A, I> {
    /// Wait for a button to be pressed.
    ///
    /// Returns `Success` when the button is pressed,
    /// otherwise it returns `Running`.
    WaitForPressed(I),
    /// Wait for a button to be released.
    ///
    /// Returns `Success` when the button is released,
    /// otherwise it returns `Running`.
    WaitForReleased(I),
    /// Waits an amount of time before continuing.
    ///
    /// f64: Time in seconds
    Wait(f64),
    /// Wait forever.
    WaitForever,
    /// A high level description of an action.
    Action(A),
    /// Converts `Success` into `Failure` and vice versa.
    Fail(Box<Behavior<A, I>>),
    /// Ignores failures and returns `Success`.
    AlwaysSucceed(Box<Behavior<A, I>>),
    /// Runs behaviors one by one until a behavior succeeds.
    ///
    /// If a behavior fails it will try the next one.
    /// Fails if the last behavior fails.
    /// Can be thought of as a short-circuited logical OR gate.
    Select(Vec<Behavior<A, I>>),
    /// `If(condition, success, failure)`
    If(
        Box<Behavior<A, I>>,
        Box<Behavior<A, I>>,
        Box<Behavior<A, I>>,
    ),
    /// Runs behaviors one by one until all succeeded.
    ///
    /// The sequence fails if a behavior fails.
    /// The sequence succeeds if all the behavior succeeds.
    /// Can be thought of as a short-circuited logical AND gate.
    Sequence(Vec<Behavior<A, I>>),
    /// Loops while conditional behavior is running.
    ///
    /// Succeeds if the conditional behavior succeeds.
    /// Fails if the conditional behavior fails,
    /// or if any behavior in the loop body fails.
    While(Box<Behavior<A, I>>, Vec<Behavior<A, I>>),
    /// Runs all behaviors in parallel until all succeeded.
    ///
    /// Succeeds if all behaviors succeed.
    /// Fails is any behavior fails.
    WhenAll(Vec<Behavior<A, I>>),
    /// Runs all behaviors in parallel until one succeeds.
    ///
    /// Succeeds if one behavior succeeds.
    /// Fails if all behaviors failed.
    WhenAny(Vec<Behavior<A, I>>),
    /// Runs all behaviors in parallel until all succeeds in sequence.
    ///
    /// Succeeds if all behaviors succeed, but only if succeeding in sequence.
    /// Fails if one behavior fails.
    After(Vec<Behavior<A, I>>),
}
