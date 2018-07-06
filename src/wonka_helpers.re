open Wonka_types;

let talkbackPlaceholder = (. _: talkbackT) => ();

let captureTalkback = (
  source: (. (. signalT('a)) => unit) => unit,
  sinkWithTalkback: (. signalT('a), (. talkbackT) => unit) => unit
) => {
  let talkback = ref(talkbackPlaceholder);
  let sink = (. signal) => {
    switch (signal) {
    | Start(x) => talkback := x
    | _ => ()
    };

    sinkWithTalkback(. signal, talkback^)
  };

  source(. sink);
};

type trampolineT = {
  mutable exhausted: bool,
  mutable inLoop: bool,
  mutable gotSignal: bool
};

let makeTrampoline = (
  sink: (. signalT('a)) => unit,
  f: (. unit) => option('a)
) => {
  let state: trampolineT = {
    exhausted: false,
    inLoop: false,
    gotSignal: false
  };

  let loop = () => {
    let rec explode = () =>
      switch (f(.)) {
      | Some(x) => {
        state.gotSignal = false;
        sink(. Push(x));
        if (state.gotSignal) explode();
      }
      | None => {
        state.exhausted = true;
        sink(. End)
      }
      };

    state.inLoop = true;
    explode();
    state.inLoop = false;
  };

  let startSignal = Start((. signal) => {
    switch (signal, state.exhausted) {
    | (Pull, false) => {
      state.gotSignal = true;
      if (!state.inLoop) loop();
    }
    | _ => ()
    }
  });

  sink(. startSignal);
};
