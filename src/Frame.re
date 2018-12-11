type contextLine = (int, string);

type frame = {
  filename: option(string),
  line: option(int),
  col: option(int),
  context: list(contextLine),
};

let getFilename = frame => frame.filename;
let getLineNumber = frame => frame.line;
let getColumnNumber = frame => frame.col;
let getContext = frame => frame.context;

let decodeFrame = json =>
  Json.Decode.{
    filename: json |> field("filename", optional(string)),
    line: json |> field("lineNo", optional(int)),
    col: json |> field("colNo", optional(int)),
    context: json |> field("context", list(tuple2(int, string))),
  };

let getCodeLineByNo = (frame, lineNo) => List.assoc(lineNo, frame.context);

let rec ind = (~i=0, line, lst) =>
  switch (lst) {
  | [] => None
  | [l, ...rest] =>
    if (String.compare(l, line) == 0) {
      Some(i);
    } else {
      ind(~i=i + 1, line, rest);
    }
  };

let fix = (fileContents, frame) =>
  switch (getLineNumber(frame)) {
  | Some(lineNo) =>
    let anchorLine = getCodeLineByNo(frame, lineNo) |> String.trim;
    let numLinesInFile = fileContents |> Array.length;

    if (numLinesInFile >= lineNo
        && String.trim(fileContents[lineNo - 1]) == anchorLine) {
      frame;
    } else {
      let minSearchLine = max(lineNo - 10, 1);
      let maxSearchLines =
        min(numLinesInFile, minSearchLine + 20) - minSearchLine;
      let searchRange =
        Array.sub(fileContents, minSearchLine - 1, maxSearchLines + 1)
        |> Array.map(String.trim)
        |> Array.to_list;

      switch (ind(anchorLine, searchRange)) {
      | Some(i) => {...frame, line: Some(minSearchLine + i)}
      | None => {...frame, line: None, col: None}
      };
    };
  | None => frame
  };
