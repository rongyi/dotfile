# 配置过程中的一些注意事项  #

## ultisnips ##
这个模板插件我是与YCM一起用的，但是发现自己机器上回车会把某个类型的所有模板列出来，造成我正常的回车操作受影响，所以我把这功能干掉了。


    --- a/plugin/UltiSnips.vim
    +++ b/plugin/UltiSnips.vim
    @@ -210,8 +210,8 @@ function! UltiSnips_MapKeys()
         exec 'xnoremap ' . g:UltiSnipsExpandTrigger. ' :call UltiSnips_SaveLastVisualSelection()<cr>gvs'
         exec "inoremap <silent> " . g:UltiSnipsJumpBackwardTrigger . " <C-R>=UltiSnips_JumpBackwards()<cr>"
         exec "snoremap <silent> " . g:UltiSnipsJumpBackwardTrigger . " <Esc>:call UltiSnips_JumpBackwards()<cr>"
    -    exec "inoremap <silent> " . g:UltiSnipsListSnippets . " <C-R>=UltiSnips_ListSnippets()<cr>"
    -    exec "snoremap <silent> " . g:UltiSnipsListSnippets . " <Esc>:call UltiSnips_ListSnippets()<cr>"
    +    "exec "inoremap <silent> " . g:UltiSnipsListSnippets . " <C-R>=UltiSnips_ListSnippets()<cr>"
    +    "exec "snoremap <silent> " . g:UltiSnipsListSnippets . " <Esc>:call UltiSnips_ListSnippets()<cr>"

         snoremap <silent> <BS> <c-g>c
         snoremap <silent> <DEL> <c-g>c


## YCM ##
YCM新版本有许多submodule，所以别忘了执行`git submodule update --init --recursive`这个命令。

YCM的依赖:Clang3.3可以用官方debian的二进制文件。如果是自己编译的话，把llvm和clang下下来之后把clang的源码解压后命名为 `clang` 丢到llvm的 `tools`文件夹下。然后和llvm一起编译。时长至少1小时。所以没功夫还是不要编了。

