type stacktrace = list(Frame.frame);

type entry =
  | Exception(string, option(string), stacktrace)
  | Other;

let isException =
  fun
  | Exception(_, _, _) => true
  | Other => false;

let getStacktrace = exc =>
  switch (exc) {
  | Exception(_, _, stacktrace) => stacktrace
  | Other => []
  };

let decodeFrames = json =>
  json |> Json.Decode.field("frames", Json.Decode.list(Frame.decodeFrame));

let decodeException = json =>
  Json.Decode.(
    json
    |> field(
         "values",
         list(json =>
           Exception(
             json |> field("type", string),
             json |> field("value", optional(string)),
             json |> field("stacktrace", decodeFrames),
           )
         ),
       )
    |> List.hd
  );

let decodeEntry = json => {
  open Json.Decode;
  let entryType = json |> field("type", string);
  switch (entryType) {
  | "exception" => json |> field("data", decodeException)
  | _ => Other
  };
};

let decodeStacktrace = json => {
  let listOfEntries = json |> Json.Decode.list(decodeEntry);
  let listOfExceptions = listOfEntries |> List.filter(isException);

  List.length(listOfExceptions) > 0 ?
    Some(listOfExceptions |> List.hd |> getStacktrace |> List.rev) : None;
};

let formatFrame = (~filename=?, frame) =>
  switch (
    filename,
    Frame.getFilename(frame),
    Frame.getLineNumber(frame),
    Frame.getColumnNumber(frame),
  ) {
  | (Some(f), _, Some(l), Some(c)) =>
    f ++ ":" ++ string_of_int(l) ++ ":" ++ string_of_int(c)
  | (None, Some(f), Some(l), Some(c)) =>
    f ++ ":" ++ string_of_int(l) ++ ":" ++ string_of_int(c)
  | _ => "anonymous"
  };

let formatStacktrace = stacktrace =>
  switch (stacktrace) {
  | Some(trace) =>
    trace
    |> List.fold_left((acc, frame) => acc ++ formatFrame(frame) ++ "\n", "")
  | None => ""
  };
