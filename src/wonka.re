open Wonka_types;
open Wonka_helpers;

module Types = Wonka_types;

let create = (gen, sink) => makeTrampoline(sink, (.) => gen());

let fromList = (l, sink) => {
  let restL = ref(l);

  makeTrampoline(sink, (.) => {
    switch (restL^) {
    | [x, ...rest] => {
      restL := rest;
      Some(x)
    }
    | [] => None
    }
  });
};

let fromArray = (a, sink) => {
  let size = Array.length(a);
  let i = ref(0);

  makeTrampoline(sink, (.) => {
    if (i^ < size) {
      let res = Some(Array.unsafe_get(a, i^));
      i := i^ + 1;
      res
    } else {
      None
    }
  });
};

let fromValue = (x, sink) => {
  let ended = ref(false);
  let talkback = Start((. signal) => {
    switch (signal) {
    | Pull when !ended^ => {
      ended := true;
      sink(. Push(x));
      sink(. End);
    }
    | _ => ()
    }
  });

  sink(. talkback);
};

let empty = () => (. sink) => {
  let talkback = (. _) => ();
  sink(. Start(talkback));
  sink(. End);
};

let never = () => (. sink) => {
  let talkback = (. _) => ();
  sink(. Start(talkback));
};

let map = (f, source, sink) => {
  let talkback = (. signal) => {
    let forward = switch (signal) {
    | Start(x) => Start(x)
    | Push(x) => Push(f(x))
    | End => End
    };

    sink(. forward);
  };

  source(. talkback);
};

let filter = (f, source, sink) =>
  captureTalkback(source, (. signal, talkback) => {
    switch (signal) {
    | Push(x) when !f(x) => talkback(. Pull)
    | _ => sink(. signal)
    }
  });

let scan = (f, seed, source, sink) => {
  let acc = ref(seed);
  let talkback = (. signal) => {
    let forward = switch (signal) {
    | Push(x) => {
      acc := f(acc^, x);
      Push(acc^)
    }
    | Start(x) => Start(x)
    | End => End
    };

    sink(. forward);
  };

  source(. talkback);
};

type mergeStateT = {
  mutable started: int,
  mutable ended: int
};

let merge = (sources, sink) => {
  let noop = talkbackPlaceholder;
  let size = Array.length(sources);
  let talkbacks = Array.map((_) => noop, sources);

  let state: mergeStateT = {
    started: 0,
    ended: 0
  };

  let talkback = (. signal) => {
    let rec loopTalkbacks = (i: int) =>
      if (i < size) {
        Array.unsafe_get(talkbacks, i)(. signal);
        loopTalkbacks(i + 1);
      };

    loopTalkbacks(0);
  };

  let rec loopSources = (i: int) =>
    if (i < size) {
      let source = Array.unsafe_get(sources, i);
      let proxySink = (. signal) => {
        switch (signal) {
        | Start(tb) => {
          Array.unsafe_set(talkbacks, i, tb);
          state.started = state.started + 1;
          if (state.started === size) sink(. Start(talkback));
        }
        | End => {
          state.ended = state.ended + 1;
          if (state.ended === size) sink(. End);
        }
        | Push(_) => sink(. signal)
        }
      };

      source(. proxySink);
      loopSources(i + 1);
    };

  loopSources(0);
};

let concat = (sources, sink) => {
  let size = Array.length(sources);
  let talkback = ref(talkbackPlaceholder);
  let rec nextSource = (i: int) =>
    if (i < size) {
      let source = Array.unsafe_get(sources, i);
      let proxySink = (. signal) => {
        switch (signal) {
        | Start(tb) => {
          talkback := tb;
          if (i === 0) {
            let currentTalkbackProxy = (. signal) => talkback^(. signal);
            sink(. Start(currentTalkbackProxy));
          };

          tb(. Pull);
        }
        | End => nextSource(i + 1)
        | Push(_) => sink(. signal)
        }
      };

      source(. proxySink);
    } else {
      sink(. End);
    };

  nextSource(0);
};

type shareStateT('a) = {
  sinks: Belt.MutableMap.Int.t((. signalT('a)) => unit),
  mutable idCounter: int,
  mutable talkback: (. talkbackT) => unit,
  mutable ended: bool,
  mutable gotSignal: bool
};

let share = source => {
  let state = {
    sinks: Belt.MutableMap.Int.make(),
    idCounter: 0,
    talkback: talkbackPlaceholder,
    ended: false,
    gotSignal: false
  };

  let proxySink = (. signal) => {
    switch (signal) {
    | Push(_) when !state.ended => {
      state.gotSignal = false;
      Belt.MutableMap.Int.forEachU(state.sinks, (. _, sink) => sink(. signal));
    }
    | Push(_) => ()
    | Start(x) => {
      state.talkback = x;
    }
    | End => {
      state.ended = true;
      Belt.MutableMap.Int.forEachU(state.sinks, (. _, sink) => sink(. End));
    }
    };
  };

  sink => {
    let id = state.idCounter;
    Belt.MutableMap.Int.set(state.sinks, id, sink);
    state.idCounter = state.idCounter + 1;

    if (id === 0) {
      source(. proxySink);
    };

    sink(. Start((. signal) => {
      switch (signal) {
      | Pull when !state.gotSignal => {
        state.gotSignal = true;
        state.talkback(. signal);
      }
      | Pull => ()
      | End => {
        Belt.MutableMap.Int.remove(state.sinks, id);
        if (Belt.MutableMap.Int.isEmpty(state.sinks)) {
          state.ended = true;
          state.talkback(. End);
        };
      }
      }
    }));
  }
};

type combineStateT('a, 'b) = {
  mutable talkbackA: (. talkbackT) => unit,
  mutable talkbackB: (. talkbackT) => unit,
  mutable lastValA: option('a),
  mutable lastValB: option('b),
  mutable gotSignal: bool,
  mutable endCounter: int,
  mutable ended: bool,
};

let combine = (sourceA, sourceB, sink) => {
  let state = {
    talkbackA: talkbackPlaceholder,
    talkbackB: talkbackPlaceholder,
    lastValA: None,
    lastValB: None,
    gotSignal: false,
    endCounter: 0,
    ended: false
  };

  sourceA(. (. signal) => {
    switch (signal, state.lastValB) {
    | (Start(tb), _) => state.talkbackA = tb
    | (Push(a), None) => {
      state.lastValA = Some(a);
      state.gotSignal = false;
    }
    | (Push(a), Some(b)) when !state.ended => {
      state.lastValA = Some(a);
      state.gotSignal = false;
      sink(. Push((a, b)));
    }
    | (End, _) when state.endCounter < 1 =>
      state.endCounter = state.endCounter + 1
    | (End, _) when !state.ended => {
      state.ended = true;
      sink(. End);
    }
    | _ => ()
    }
  });

  sourceB(. (. signal) => {
    switch (signal, state.lastValA) {
    | (Start(tb), _) => state.talkbackB = tb
    | (Push(b), None) => {
      state.lastValB = Some(b);
      state.gotSignal = false;
    }
    | (Push(b), Some(a)) when !state.ended => {
      state.lastValB = Some(b);
      state.gotSignal = false;
      sink(. Push((a, b)));
    }
    | (End, _) when state.endCounter < 1 =>
      state.endCounter = state.endCounter + 1
    | (End, _) when !state.ended => {
      state.ended = true;
      sink(. End);
    }
    | _ => ()
    }
  });

  sink(. Start((. signal) => {
    if (!state.ended) {
      switch (signal) {
      | Pull when !state.gotSignal => {
        state.gotSignal = true;
        state.talkbackA(. signal);
        state.talkbackB(. signal);
      }
      | Pull => ()
      | End => {
        state.ended = true;
        state.talkbackA(. End);
        state.talkbackB(. End);
      }
      }
    };
  }));
};

type takeStateT = {
  mutable taken: int,
  mutable talkback: (. talkbackT) => unit
};

let take = (max, source, sink) => {
  let state: takeStateT = {
    taken: 0,
    talkback: talkbackPlaceholder
  };

  source(. (. signal) => {
    switch (signal) {
    | Start(tb) => state.talkback = tb;
    | Push(_) when state.taken < max => {
      state.taken = state.taken + 1;
      sink(. signal);

      if (state.taken === max) {
        sink(. End);
        state.talkback(. End);
      };
    }
    | Push(_) => ()
    | End when state.taken < max => {
      state.taken = max;
      sink(. End)
    }
    | End => ()
    }
  });

  sink(. Start((. signal) => {
    if (state.taken < max) {
      switch (signal) {
      | Pull => state.talkback(. Pull);
      | End => {
        state.taken = max;
        state.talkback(. End);
      }
      }
    };
  }));
};

let takeLast = (max, source, sink) => {
  let queue = Belt.MutableQueue.make();

  captureTalkback(source, (. signal, talkback) => {
    switch (signal) {
    | Start(_) => talkback(. Pull)
    | Push(x) => {
      let size = Belt.MutableQueue.size(queue);
      if (size >= max && max > 0) {
        ignore(Belt.MutableQueue.pop(queue));
      };

      Belt.MutableQueue.add(queue, x);
      talkback(. Pull);
    }
    | End => makeTrampoline(sink, [@bs] () => Belt.MutableQueue.pop(queue))
    }
  });
};

let takeWhile = (predicate, source, sink) => {
  let ended = ref(false);
  let talkback = ref(talkbackPlaceholder);
  let proxySink = (. signal) => {
    switch (signal) {
    | Start(tb) => {
      talkback := tb;
      sink(. signal);
    }
    | End when !ended^ => {
      ended := true;
      sink(. End);
    }
    | End => ()
    | Push(x) when !ended^ => {
      if (!predicate(x)) {
        ended := true;
        sink(. End);
        talkback^(. End);
      } else {
        sink(. signal);
      };
    }
    | Push(_) => ()
    }
  };

  source(. proxySink);

  sink(. Start((. signal) => {
    if (!ended^) {
      switch (signal) {
      | Pull => talkback^(. Pull);
      | End => {
        ended := true;
        talkback^(. End);
      }
      }
    };
  }));
};

type takeUntilStateT = {
  mutable ended: bool,
  mutable sourceTalkback: (. talkbackT) => unit,
  mutable notifierTalkback: (. talkbackT) => unit
};

let takeUntil = (notifier, source, sink) => {
  let state: takeUntilStateT = {
    ended: false,
    sourceTalkback: talkbackPlaceholder,
    notifierTalkback: talkbackPlaceholder
  };

  let proxySink = (. signal) => {
    switch (signal) {
    | Start(tb) => {
      state.sourceTalkback = tb;

      notifier(. (. signal) => {
        switch (signal) {
        | Start(innerTb) => {
          state.notifierTalkback = innerTb;
          innerTb(. Pull);
        }
        | Push(_) => {
          state.ended = true;
          state.notifierTalkback(. End);
          state.sourceTalkback(. End);
          sink(. End);
        }
        | End => ()
        }
      });
    }
    | End when !state.ended => {
      state.notifierTalkback(. End);
      state.ended = true;
      sink(. End);
    }
    | End => ()
    | Push(_) when !state.ended => sink(. signal)
    | Push(_) => ()
    }
  };

  source(. proxySink);

  sink(. Start((. signal) => {
    if (!state.ended) {
      switch (signal) {
      | Pull => state.sourceTalkback(. Pull)
      | End => {
        state.sourceTalkback(. End);
        state.notifierTalkback(. End);
      }
      }
    };
  }));
};

let skip = (wait, source, sink) => {
  let rest = ref(wait);

  captureTalkback(source, (. signal, talkback) => {
    switch (signal) {
    | Push(_) when rest^ > 0 => {
      rest := rest^ - 1;
      talkback(. Pull);
    }
    | _ => sink(. signal)
    }
  });
};

let skipWhile = (predicate, source, sink) => {
  let skip = ref(true);

  captureTalkback(source, (. signal, talkback) => {
    switch (signal) {
    | Push(x) when skip^ => {
      if (predicate(x)) {
        talkback(. Pull);
      } else {
        skip := false;
        sink(. signal);
      };
    }
    | _ => sink(. signal)
    }
  });
};

type skipUntilStateT = {
  mutable skip: bool,
  mutable ended: bool,
  mutable gotSignal: bool,
  mutable sourceTalkback: (. talkbackT) => unit,
  mutable notifierTalkback: (. talkbackT) => unit
};

let skipUntil = (notifier, source, sink) => {
  let state: skipUntilStateT = {
    skip: true,
    ended: false,
    gotSignal: false,
    sourceTalkback: talkbackPlaceholder,
    notifierTalkback: talkbackPlaceholder
  };

  let proxySink = (. signal) => {
    switch (signal) {
    | Start(tb) => {
      state.sourceTalkback = tb;

      notifier(. (. signal) => {
        switch (signal) {
        | Start(innerTb) => {
          state.notifierTalkback = innerTb;
          innerTb(. Pull);
          tb(. Pull);
        }
        | Push(_) => {
          state.skip = false;
          state.notifierTalkback(. End);
        }
        | End => ()
        }
      });
    }
    | Push(_) when state.skip && !state.ended => {
      state.sourceTalkback(. Pull);
    }
    | Push(_) when !state.ended => {
      state.gotSignal = false;
      sink(. signal);
    }
    | Push(_) => ()
    | End => {
      if (state.skip) state.notifierTalkback(. End);
      state.ended = true;
      sink(. End);
    }
    }
  };

  source(. proxySink);

  sink(. Start((. signal) => {
    switch (signal) {
    | Pull when !state.gotSignal && !state.ended => {
      state.gotSignal = true;
      state.sourceTalkback(. Pull);
    }
    | Pull => ()
    | End => {
      if (state.skip) state.notifierTalkback(. End);
      state.ended = true;
      state.sourceTalkback(. End);
    }
    }
  }));
};

type flattenStateT = {
  mutable sourceTalkback: (. talkbackT) => unit,
  mutable innerTalkback: (. talkbackT) => unit,
  mutable sourceEnded: bool,
  mutable innerEnded: bool
};

let flatten = (source, sink) => {
  let state: flattenStateT = {
    sourceTalkback: talkbackPlaceholder,
    innerTalkback: talkbackPlaceholder,
    sourceEnded: false,
    innerEnded: true
  };

  let applyInnerSource = (. innerSource: (. (. signalT('a)) => unit) => unit) => {
    innerSource(. (. signal) => {
      switch (signal) {
      | Start(tb) => {
        if (!state.innerEnded) {
          state.innerTalkback(. End);
        };

        state.innerEnded = false;
        state.innerTalkback = tb;
        tb(. Pull);
      }
      | End when !state.sourceEnded => {
        state.innerEnded = true;
        state.sourceTalkback(. Pull);
      }
      | End => state.sourceTalkback(. End)
      | Push(_) => sink(. signal)
      };
    });
  };

  let proxySink = (. signal) => {
    switch (signal) {
    | Start(tb) => {
      state.sourceTalkback = tb;
    }
    | Push(innerSource) => applyInnerSource(. innerSource)
    | End when !state.innerEnded => {
      state.sourceEnded = true;
    }
    | End => sink(. End)
    };
  };

  source(. proxySink);

  sink(. Start((. signal) => {
    switch (signal) {
    | Pull when !state.innerEnded && !state.sourceEnded => {
      state.innerTalkback(. Pull);
    }
    | Pull when !state.sourceEnded => {
      state.sourceTalkback(. Pull);
    }
    | Pull => ()
    | End => {
      state.sourceTalkback(. End);
      state.innerTalkback(. End);
    }
    }
  }));
};

let forEach = (f, source) => {
  let talkback = ref(talkbackPlaceholder);
  let ended = ref(false);

  source(. (. signal) => {
    switch (signal) {
    | Start(x) => {
      talkback := x;
      talkback^(. Pull);
    }
    | Push(x) when !ended^ => {
      f(x);
      talkback^(. Pull);
    }
    | _ => ()
    }
  });

  () => if (!ended^) {
    ended := true;
    talkback^(. End);
  }
};

let subscribe = (observer, source) => {
  let talkback = ref(talkbackPlaceholder);
  let ended = ref(false);

  source(. (. signal) => {
    switch (signal) {
    | Start(x) => {
      talkback := x;
      talkback^(. Pull);
    }
    | Push(x) when !ended^ => {
      observer#next(x);
      talkback^(. Pull);
    }
    | End when !ended^ => {
      observer#complete();
    }
    | _ => ()
    }
  });

  () => if (!ended^) {
    ended := true;
    talkback^(. End);
    observer#complete();
  }
};
