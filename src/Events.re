module Metadata = {
  type metadata = {
    value: string,
    filename: option(string),
  };

  let decode = json =>
    Json.Decode.{
      value: json |> field("value", string),
      filename: json |> optional(field("filename", Json.Decode.string)),
    };

  let getFilename = metadata =>
    switch (metadata.filename) {
    | Some(fname) => fname
    | None => ""
    };

  let getMessage = metadata => metadata.value;
};

type event = {
  id: string,
  message: string,
  filename: string,
  stacktrace: option(Stacktrace.stacktrace),
};

let readEventMessage = json => {
  open Json.Decode;
  let metadataMessage =
    json |> field("metadata", Metadata.decode) |> Metadata.getMessage;

  switch (metadataMessage) {
  | m when m != "" => m
  | _ => json |> field("message", string)
  };
};

let decodeEvent = json =>
  Json.Decode.{
    id: json |> field("id", string),
    message: json |> readEventMessage,
    filename:
      json |> field("metadata", Metadata.decode) |> Metadata.getFilename,
    stacktrace: json |> field("entries", Stacktrace.decodeStacktrace),
  };

let decodeEventList = json => json |> Json.Decode.list(decodeEvent);

let readEvent = json => Json.parseOrRaise(json) |> decodeEvent;
let readEvents = json => Json.parseOrRaise(json) |> decodeEventList;

let fetchEvents = (issueId, authToken) =>
  Js.Promise.(
    Fetch.fetchWithInit(
      "https://sentry.io/api/0/issues/" ++ issueId ++ "/events/",
      Fetch.RequestInit.make(
        ~headers=
          Fetch.HeadersInit.make({"Authorization": "Bearer " ++ authToken}),
        (),
      ),
    )
    |> then_(Fetch.Response.text)
    |> then_(json => readEvents(json) |> resolve)
  );

let fetchLatestEvent = (issueId, authToken) =>
  Js.Promise.(
    Fetch.fetchWithInit(
      "https://sentry.io/api/0/issues/" ++ issueId ++ "/events/latest/",
      Fetch.RequestInit.make(
        ~headers=
          Fetch.HeadersInit.make({"Authorization": "Bearer " ++ authToken}),
        (),
      ),
    )
    |> then_(Fetch.Response.text)
    |> then_(json => readEvent(json) |> resolve)
  );

let getId = event => event.id;
let getMessage = event => event.message;
let getFilename = event => event.filename;
let getStacktrace = event => event.stacktrace;
