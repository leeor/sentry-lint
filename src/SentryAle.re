[%raw "require('isomorphic-fetch')"];

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

let formatContextLine = contextLine =>
  Printf.sprintf(
    format_of_string("% 5i | %s"),
    fst(contextLine),
    snd(contextLine),
  );

let printContext = context =>
  context |> List.map(formatContextLine) |> String.concat("\n");

let rec range = (from, _to) =>
  if (from <= _to) {
    [from, ...range(from + 1, _to)];
  } else {
    [];
  };

let formatEslintCompatibleMessage = (~filename=?, fileContents, issue, event) => {
  let filename =
    switch (filename, Issues.getFilename(issue), Events.getFilename(event)) {
    | (Some(_), _, _) => filename
    | (None, issueFilename, _) when issueFilename != "" =>
      Some(issueFilename)
    | (None, _, eventFilename) when eventFilename != "" =>
      Some(eventFilename)
    | _ => None
    };

  let frame =
    switch (Events.getStacktrace(event)) {
    | Some(stacktrace) =>
      Some(stacktrace |> List.hd |> Frame.fix(fileContents))
    | None => None
    };

  let lineNo =
    switch (frame) {
    | Some(f) => Frame.getLineNumber(f)
    | None => None
    };

  let colNo =
    switch (frame) {
    | Some(f) => Frame.getColumnNumber(f)
    | None => None
    };

  let message =
    (Events.getMessage(event) |> Js.String.split("\n") |> Array.get(_, 0))
    ++ " [Error/"
    ++ Issues.getPermalink(issue)
    ++ "]";

  let context =
    switch (lineNo) {
    | Some(line) =>
      let lineFrom = max(line - 5, 1);
      let lineTo = min(fileContents |> Array.length, line + 5);

      Array.sub(fileContents, lineFrom - 1, lineTo - lineFrom + 1)
      |> Array.to_list
      |> List.combine(range(lineFrom, lineTo))
      |> printContext;
    | _ => ""
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

let fileContents =
  Node.Fs.readFileSync("/dev/stdin", `ascii) |> Js.String.split("\n");

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
               Js.log(
                 formatEslintCompatibleMessage(
                   ~filename=fileName^,
                   fileContents,
                   issue,
                   event,
                 ),
               )
               |> resolve
             )
          |> catch(err => Js.log(err) |> resolve)
        )
     |> Array.of_list
     |> all
   );
