type contextLine = (int, string);

let getLineNumber = cline => fst(cline);
let getLineCode = cline => snd(cline);
let formatContextLine = cline =>
  Printf.sprintf(
    format_of_string("% 5i | %s"),
    getLineNumber(cline),
    getLineCode(cline),
  );

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
