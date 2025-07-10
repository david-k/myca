cd <sfile>:h

args src/*
edit src/main.cpp

let g:vimspector_configurations = {
  \   'C++ Debug': {
  \     'adapter': 'vscode-cpptools',
  \     'configuration': {
  \       'request': 'launch',
  \       'program': '${cwd}/build/myca',
  \       'args': ["examples/try.myca", "-o", "build/examples/try.c"],
  \       'stopAtEntry': v:false,
  \       'cwd': '${cwd}',
  \       'environment': [],
  \       'externalConsole': v:true,
  \       "MIMode": "gdb",
  \       "setupCommands": [
  \         {
  \           "description": "Enable pretty-printing for gdb",
  \           "text": "-enable-pretty-printing",
  \           "ignoreFailures": v:true
  \         }
  \       ]
  \     },
  \   }
  \ }
