module Metadata = {
  type metadata = {filename: option(string)};

  let decode = json =>
    Json.Decode.{
      filename: json |> optional(field("filename", Json.Decode.string)),
    };

  let getFilename = metadata =>
    switch (metadata.filename) {
    | Some(fname) => fname
    | None => ""
    };
};

type issue = {
  title: string,
  userCount: int,
  id: string,
  filename: string,
  permalink: string,
};

let decodeIssue = json =>
  Json.Decode.{
    title: json |> field("title", string),
    id: json |> field("id", string),
    userCount: json |> field("userCount", int),
    filename:
      json |> field("metadata", Metadata.decode) |> Metadata.getFilename,
    permalink: json |> field("permalink", string),
  };

let decodeIssueList = json => json |> Json.Decode.list(decodeIssue);

let readIssues = json => Json.parseOrRaise(json) |> decodeIssueList;

let fetchIssues = (orgSlug, projectSlug, authToken) =>
  Js.Promise.(
    Fetch.fetchWithInit(
      "https://sentry.io/api/0/projects/"
      ++ orgSlug
      ++ "/"
      ++ projectSlug
      ++ "/issues/?query=is:unresolved+level:error",
      Fetch.RequestInit.make(
        ~headers=
          Fetch.HeadersInit.make({"Authorization": "Bearer " ++ authToken}),
        (),
      ),
    )
    |> then_(Fetch.Response.text)
    |> then_(json => readIssues(json) |> resolve)
    |> then_(issues =>
         issues
         |> List.filter(issue =>
              String.length(issue.filename) > 2
              && String.sub(issue.filename, 0, 2) == "./"
            )
         |> resolve
       )
  );

let getTitle = issue => issue.title;
let getUserCount = issue => issue.userCount;
let getId = issue => issue.id;
let getFilename = issue => issue.filename;
let getPermalink = issue => issue.permalink;
