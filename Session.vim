let SessionLoad = 1
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
silent only
silent tabonly
cd ~/Working/Functional-Fundamentals
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +23 Basics/arithmetic.hs
badd +1 Basics/list1.scm
badd +51 Basics/arithmetic.scm
badd +1 term://~/Working/Coding/Languages/HaScheme/Functional-Fundamentals//223233:/usr/bin/fish
badd +1 term://~/Working/Functional-Fundamentals//256617:/usr/bin/fish
badd +4 Basics/arithmetic.md
badd +0 term://~/Working/Functional-Fundamentals//294812:/usr/bin/fish
argglobal
%argdel
edit Basics/arithmetic.hs
let s:save_splitbelow = &splitbelow
let s:save_splitright = &splitright
set splitbelow splitright
wincmd _ | wincmd |
vsplit
wincmd _ | wincmd |
vsplit
2wincmd h
wincmd w
wincmd w
wincmd _ | wincmd |
split
1wincmd k
wincmd w
let &splitbelow = s:save_splitbelow
let &splitright = s:save_splitright
wincmd t
let s:save_winminheight = &winminheight
let s:save_winminwidth = &winminwidth
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
exe 'vert 1resize ' . ((&columns * 56 + 85) / 170)
exe 'vert 2resize ' . ((&columns * 56 + 85) / 170)
exe '3resize ' . ((&lines * 30 + 19) / 38)
exe 'vert 3resize ' . ((&columns * 56 + 85) / 170)
exe '4resize ' . ((&lines * 5 + 19) / 38)
exe 'vert 4resize ' . ((&columns * 56 + 85) / 170)
argglobal
balt Basics/list1.scm
let s:l = 23 - ((17 * winheight(0) + 18) / 36)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 23
normal! 03|
wincmd w
argglobal
if bufexists("Basics/arithmetic.scm") | buffer Basics/arithmetic.scm | else | edit Basics/arithmetic.scm | endif
if &buftype ==# 'terminal'
  silent file Basics/arithmetic.scm
endif
balt Basics/arithmetic.hs
let s:l = 49 - ((25 * winheight(0) + 18) / 36)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 49
normal! 019|
wincmd w
argglobal
if bufexists("Basics/arithmetic.md") | buffer Basics/arithmetic.md | else | edit Basics/arithmetic.md | endif
if &buftype ==# 'terminal'
  silent file Basics/arithmetic.md
endif
balt term://~/Working/Functional-Fundamentals//256617:/usr/bin/fish
let s:l = 4 - ((3 * winheight(0) + 15) / 30)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 4
normal! 0
wincmd w
argglobal
if bufexists("term://~/Working/Functional-Fundamentals//294812:/usr/bin/fish") | buffer term://~/Working/Functional-Fundamentals//294812:/usr/bin/fish | else | edit term://~/Working/Functional-Fundamentals//294812:/usr/bin/fish | endif
if &buftype ==# 'terminal'
  silent file term://~/Working/Functional-Fundamentals//294812:/usr/bin/fish
endif
balt Basics/arithmetic.md
let s:l = 294 - ((4 * winheight(0) + 2) / 5)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 294
normal! 017|
wincmd w
4wincmd w
exe 'vert 1resize ' . ((&columns * 56 + 85) / 170)
exe 'vert 2resize ' . ((&columns * 56 + 85) / 170)
exe '3resize ' . ((&lines * 30 + 19) / 38)
exe 'vert 3resize ' . ((&columns * 56 + 85) / 170)
exe '4resize ' . ((&lines * 5 + 19) / 38)
exe 'vert 4resize ' . ((&columns * 56 + 85) / 170)
tabnext 1
if exists('s:wipebuf') && len(win_findbuf(s:wipebuf)) == 0&& getbufvar(s:wipebuf, '&buftype') isnot# 'terminal'
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20 shortmess=filnxtToOFA
let &winminheight = s:save_winminheight
let &winminwidth = s:save_winminwidth
let s:sx = expand("<sfile>:p:r")."x.vim"
if filereadable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &g:so = s:so_save | let &g:siso = s:siso_save
set hlsearch
nohlsearch
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
