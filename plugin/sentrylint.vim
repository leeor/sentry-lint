let s:script_folder_path = escape(expand('<sfile>:p:h'), '\')

function! SentryGetCommand(buffer) abort
  let s:rc_file = ale#path#FindNearestFile(a:buffer, '.sentryrc')

  if !empty(s:rc_file) && filereadable(s:rc_file)
    let s:project = readfile(s:rc_file)[0]

    return 'node '
          \ . s:script_folder_path . '/../lib/js/src/SentryLint.js'
          \ . ' -project ' . s:project
          \ . ' %s'
  endif

  return ''
endfunction

call ale#linter#Define('javascript', {
\ 'name': 'sentry',
\ 'executable': 'node',
\ 'command_callback': 'SentryGetCommand',
\ 'callback': 'ale#handlers#eslint#Handle',
\ })
