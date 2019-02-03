# Sentry-based Linter

This tool is similar to `eslint` but the output is based on error reports logged to
[sentry.io](https://sentry.io)'s.

Each error report provides a link to the issue on Sentry.

## VIM Plugin

Using `Plug`, add the following to your `vim.rc`:
```vim
Plug 'leeor/sentry-lint', { 'do': 'npm i && npm run build' }
```

Note: `node` and `npm` are required to build and run this tool.

## Configuration

### Global, Per User

Create a file named `.sentrylint.json` in your home directory, with the following schema:

```json
{
  "project-name": {
    "org": "<sentry organization name>",
    "token": "<Sentry API authorization token>"
  }
}
```

### Project

In the root of each project add a `.sentryrc` file that contains the name of the project in Sentry:

```text
project-name
```
