open Wonka_types;

/* -- source factories */

/* Accepts a period in milliseconds and creates a listenable source
   that emits ascending numbers for each time the interval fires.
   This stream will emit values indefinitely until it receives an
   End signal from a talkback passed downwards to its sink. */
let interval: (
  int,
  (. signalT(int)) => unit
) => unit;

/* Accepts a JS promise and creates a listenable source that emits
   the promise's value once it resolves.
   This stream will wait for the promise's completion, unless it
   receives an End signal first. */
let fromPromise: (
  Js.Promise.t('a),
  (. signalT('a)) => unit
) => unit;

/* -- operators */

/* Takes a projection to a period in milliseconds and a source, and creates
   a listenable source that emits the last emitted value if no other value
   has been emitted during the passed debounce period. */
let debounce: (
  'a => int,
  (. (. signalT('a)) => unit) => unit,
  (. signalT('a)) => unit
) => unit;

/* Takes a projection to a period in milliseconds and a source, and creates
   a listenable source that ignores values after the last emitted value for
   the duration of the returned throttle period. */
let throttle: (
  'a => int,
  (. (. signalT('a)) => unit) => unit,
  (. signalT('a)) => unit
) => unit;

/* Takes a notifier source and an input source, and creates a sink & source.
   When the notifier emits a value, it will emit the value that it most recently
   received from the input source, unless said source hasn't emitted anything
   since the last signal. */
let sample: (
  (. (. signalT('a)) => unit) => unit,
  (. (. signalT('b)) => unit) => unit,
  (. signalT('b)) => unit
) => unit;

/* Takes a projection to a period in milliseconds and a source, and creates
   a listenable source that delays every emission by that passed period. */
let delay: (
  int,
  (. (. signalT('a)) => unit) => unit,
  (. signalT('a)) => unit
) => unit;
