let g:syntastic_c_include_dirs = ['compiler/include', 'solver/include']
let g:deoplete#sources#clang#flags = ['-Icompiler/include', '-Isolver/include']

set path+=compiler/include
set path+=solver/include
