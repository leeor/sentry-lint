[%raw "require('isomorphic-fetch')"];

[@bs.val] external log: string => unit = "console.log";

type configType = {verbose: bool};

let rec getAbsolutePathPart = filename =>
  if (filename.[0] == '.') {
    getAbsolutePathPart(
      String.sub(filename, 1, String.length(filename) - 1),
    );
  } else {
    filename;
  };

let isMatchingFilename = (inputFile, filename) => {
  let absPathPart = getAbsolutePathPart(filename);
  let fileNameLength = String.length(inputFile);
  let absPathPartLength = String.length(absPathPart);

  fileNameLength >= absPathPartLength
  && String.compare(
       String.sub(
         inputFile,
         fileNameLength - absPathPartLength,
         absPathPartLength,
       ),
       absPathPart,
     )
  == 0;
};

let printFrame = frame =>
  Frame.getContext(frame)
  |> List.map(Frame.formatContextLine)
  |> String.concat("\n");

let formatEslintCompatibleMessage = (~filename=?, issue, event) => {
  let filename =
    switch (filename, Issues.getFilename(issue), Events.getFilename(event)) {
    | (Some(_), _, _) => filename
    | (None, issueFilename, _) when issueFilename != "" =>
      Some(issueFilename)
    | (None, _, eventFilename) when eventFilename != "" =>
      Some(eventFilename)
    | _ => None
    };

  let lineNo =
    switch (Events.getStacktrace(event)) {
    | Some(stacktrace) =>
      stacktrace |> List.hd |> (frame => Frame.getLineNumber(frame))
    | None => None
    };

  let colNo =
    switch (Events.getStacktrace(event)) {
    | Some(stacktrace) =>
      stacktrace |> List.hd |> (frame => Frame.getColumnNumber(frame))
    | None => None
    };

  let message =
    (Events.getMessage(event) |> Js.String.split("\n") |> Array.get(_, 0))
    ++ " [Error/"
    ++ Issues.getPermalink(issue)
    ++ "]";

  let context =
    switch (Events.getStacktrace(event)) {
    | Some(stacktrace) =>
      stacktrace |> List.hd |> (frame => printFrame(frame))
    | None => ""
    };

  switch (filename, lineNo, colNo) {
  | (Some(file), Some(line), Some(column)) =>
    file
    ++ ":"
    ++ string_of_int(line)
    ++ ":"
    ++ string_of_int(column)
    ++ ": "
    ++ message
    ++ "\n"
    ++ context
  | _ => ""
  };
};

let fileName = ref("");
let orgSlug = ref("");
let projectSlug = ref("");
let authToken = ref("");
let config = ref({verbose: false});

Arg.parse(
  [
    (
      "-verbose",
      Arg.Unit(() => config := {verbose: true}),
      "verbose output",
    ),
    ("-org", Arg.String(slug => orgSlug := slug), "organisation slug"),
    ("-project", Arg.String(slug => projectSlug := slug), "project slug"),
    (
      "-token",
      Arg.String(token => authToken := token),
      "authentication token",
    ),
  ],
  fName => {
    fileName := fName;
    ();
  },
  "",
);

open Js.Promise;

Issues.fetchIssues(orgSlug^, projectSlug^, authToken^)
|> then_(issues =>
     issues
     |> List.filter(issue =>
          issue |> Issues.getFilename |> isMatchingFilename(fileName^)
        )
     |> List.map(issue =>
          Events.fetchLatestEvent(Issues.getId(issue), authToken^)
          |> then_(event =>
               log(
                 formatEslintCompatibleMessage(
                   ~filename=fileName^,
                   issue,
                   event,
                 ),
               )
               |> resolve
             )
        )
     |> Array.of_list
     |> all
   );
