##################
# Transient Prompt
##################
#
function _print_history_separator() {
  # Define explicit color
  local separator_color=7

  # Disable line on first run
  if [[ -z $_IS_FIRST_PROMPT ]]; then
    _IS_FIRST_PROMPT=1
    return
  fi
  # Print a line that covers the whole line on transient prompt
  # Color 7 (Foreground)
  print -P "%F{${separator_color}}${(r:$COLUMNS::─:)_}%f"
}

# register hook for that transient prompt
autoload -Uz add-zsh-hook
add-zsh-hook precmd _print_history_separator


#############################
# Powerlevel10K configuration
#############################
#
'builtin' 'local' '-a' 'p10k_config_opts'
[[ ! -o 'aliases'         ]] || p10k_config_opts+=('aliases')
[[ ! -o 'sh_glob'         ]] || p10k_config_opts+=('sh_glob')
[[ ! -o 'no_brace_expand' ]] || p10k_config_opts+=('no_brace_expand')
'builtin' 'setopt' 'no_aliases' 'no_sh_glob' 'brace_expand'

() {
  emulate -L zsh -o extended_glob
  unset -m '(POWERLEVEL9K_*|DEFAULT_USER)~POWERLEVEL9K_GITSTATUS_DIR'
  [[ $ZSH_VERSION == (5.<1->*|<6->.*) ]] || return

  # ===============================================================
  # 0. COLOR PALETTE
  # ===============================================================
  local color_fg=0
  local color_bg=7

  local color_red=1
  local color_green=2
  local color_yellow=3
  local color_blue=4
  local color_purple=5
  local color_cyan=6
  local color_grey=8

  # ===============================================================
  # 1. STRUCTURE
  # ===============================================================
  typeset -g POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(
    os_icon
    user
    dir
    vcs
    newline
    prompt_char
  )

  typeset -g POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(
    kubecontext
    terraform_version
  )

  # ===============================================================
  # 2. DESIGN & FRAME
  # ===============================================================
  typeset -g POWERLEVEL9K_MODE=nerdfont-v3
  typeset -g POWERLEVEL9K_ICON_PADDING=moderate

  # Disable standard newline because we inject it manually in the prefix below
  typeset -g POWERLEVEL9K_PROMPT_ADD_NEWLINE=false

  # \n to add an empty line above the transient prompt
  typeset -g POWERLEVEL9K_MULTILINE_FIRST_PROMPT_PREFIX=$'\n%'"${color_grey}F╭─"
  typeset -g POWERLEVEL9K_MULTILINE_NEWLINE_PROMPT_PREFIX="%${color_grey}F├─"
  typeset -g POWERLEVEL9K_MULTILINE_LAST_PROMPT_PREFIX="%${color_grey}F╰─"

  # Left Separators (Rounded)
  typeset -g POWERLEVEL9K_LEFT_PROMPT_FIRST_SEGMENT_START_SYMBOL='\uE0B6'
  typeset -g POWERLEVEL9K_LEFT_SEGMENT_SEPARATOR='\uE0B0'
  typeset -g POWERLEVEL9K_LEFT_PROMPT_LAST_SEGMENT_END_SYMBOL='\uE0B0'

  # Right Separators (None)
  typeset -g POWERLEVEL9K_RIGHT_SEGMENT_SEPARATOR=''
  typeset -g POWERLEVEL9K_RIGHT_PROMPT_FIRST_SEGMENT_START_SYMBOL=''
  typeset -g POWERLEVEL9K_RIGHT_PROMPT_LAST_SEGMENT_END_SYMBOL=''

  # ===============================================================
  # 3. TRANSIENT PROMPT
  # ===============================================================
  typeset -g POWERLEVEL9K_TRANSIENT_PROMPT=always

  # ===============================================================
  # 4. MODULE CONFIGURATION & COLORS
  # ===============================================================

  # --- OS ICON ---
  typeset -g POWERLEVEL9K_OS_ICON_FOREGROUND=$color_fg
  typeset -g POWERLEVEL9K_OS_ICON_BACKGROUND=$color_grey
  typeset -g POWERLEVEL9K_OS_ICON_CONTENT_EXPANSION='%B${P9K_CONTENT}'

  # --- USER ---
  typeset -g POWERLEVEL9K_USER_FOREGROUND=$color_fg
  typeset -g POWERLEVEL9K_USER_BACKGROUND=$color_grey
  typeset -g POWERLEVEL9K_USER_FORMAT='%n'

  # --- DIRECTORY ---
  typeset -g POWERLEVEL9K_DIR_FOREGROUND=$color_fg
  typeset -g POWERLEVEL9K_DIR_BACKGROUND=$color_grey
  typeset -g POWERLEVEL9K_SHORTEN_STRATEGY=truncate_to_last
  typeset -g POWERLEVEL9K_SHORTEN_DIR_LENGTH=3
  typeset -g POWERLEVEL9K_SHORTEN_DELIMITER='.../'
  typeset -g POWERLEVEL9K_DIR_CLASSES=() # Disable dir icons
  typeset -g POWERLEVEL9K_DIR_HYPERLINK=false

  # --- GIT (VCS) - Custom Formatter ---

  # 1. Disable default formatting
  typeset -g POWERLEVEL9K_VCS_DISABLE_GITSTATUS_FORMATTING=true

  # 2. Hide default visual identifier
  typeset -g POWERLEVEL9K_VCS_VISUAL_IDENTIFIER_EXPANSION=''

  # 3. Colors
  typeset -g POWERLEVEL9K_VCS_CLEAN_BACKGROUND=$color_cyan
  typeset -g POWERLEVEL9K_VCS_CLEAN_FOREGROUND=$color_fg
  typeset -g POWERLEVEL9K_VCS_MODIFIED_BACKGROUND=$color_yellow
  typeset -g POWERLEVEL9K_VCS_MODIFIED_FOREGROUND=$color_fg
  typeset -g POWERLEVEL9K_VCS_UNTRACKED_BACKGROUND=$color_yellow
  typeset -g POWERLEVEL9K_VCS_UNTRACKED_FOREGROUND=$color_fg
  typeset -g POWERLEVEL9K_VCS_LOADING_BACKGROUND=$color_yellow
  typeset -g POWERLEVEL9K_VCS_LOADING_FOREGROUND=$color_fg

  # 4. Custom Git Formatter
  function my_git_formatter() {
    emulate -L zsh
    if [[ -n $P9K_CONTENT ]]; then
      typeset -g my_git_format=$P9K_CONTENT
      return
    fi

    local res=""

    # -- BRANCH --
    res+=" ${VCS_STATUS_LOCAL_BRANCH}"

    # -- AHEAD / BEHIND --
    (( VCS_STATUS_COMMITS_AHEAD )) && res+="  ${VCS_STATUS_COMMITS_AHEAD}"
    (( VCS_STATUS_COMMITS_BEHIND )) && res+="  ${VCS_STATUS_COMMITS_BEHIND}"

    # -- ACTION --
    if [[ -n $VCS_STATUS_ACTION ]]; then
        local action_icon=""

        case $VCS_STATUS_ACTION in
            (rebase*)      action_icon=' ' ;;
            (bisect*)      action_icon='󰃻 ' ;;
            (cherry-pick*) action_icon=' ' ;;
        esac

        res+=" ${action_icon}${VCS_STATUS_ACTION}"

        # -- PROGRESSION --
        if [[ -n $VCS_STATUS_ACTION_CP ]]; then
            res+=" ( ${VCS_STATUS_ACTION_CP} of ${VCS_STATUS_ACTION_TP} )"
        fi
    fi

    typeset -g my_git_format=$res
  }
  functions -M my_git_formatter 2>/dev/null

  # 5. Attach formatter
  typeset -g POWERLEVEL9K_VCS_CONTENT_EXPANSION='${$((my_git_formatter(0)))+${my_git_format}}'
  typeset -g POWERLEVEL9K_VCS_LOADING_CONTENT_EXPANSION='${$((my_git_formatter(0)))+${my_git_format}}'


  # --- KUBERNETES ---
  typeset -g POWERLEVEL9K_KUBECONTEXT_BACKGROUND=
  typeset -g POWERLEVEL9K_KUBECONTEXT_FOREGROUND=$color_blue # Kubernetes Blue
  typeset -g POWERLEVEL9K_KUBECONTEXT_ICON_BEFORE_CONTENT=true
  typeset -g POWERLEVEL9K_KUBECONTEXT_CONTENT_EXPANSION='${P9K_KUBECONTEXT_CLOUD_CLUSTER:+gke-${P9K_KUBECONTEXT_CLOUD_CLUSTER}}${P9K_KUBECONTEXT_CLOUD_CLUSTER:-${P9K_KUBECONTEXT_NAME}} / ${P9K_KUBECONTEXT_NAMESPACE}'
  typeset -g POWERLEVEL9K_KUBECONTEXT_SHOW_ON_COMMAND='k|kubectl|k9s|flux|helm'

  # --- TERRAFORM VERSION ---
  typeset -g POWERLEVEL9K_TERRAFORM_VERSION_BACKGROUND=
  typeset -g POWERLEVEL9K_TERRAFORM_VERSION_FOREGROUND=$color_purple
  typeset -g POWERLEVEL9K_TERRAFORM_VERSION_VISUAL_IDENTIFIER_EXPANSION='󱁢'
  typeset -g POWERLEVEL9K_TERRAFORM_VERSION_ICON_BEFORE_CONTENT=true
  typeset -g POWERLEVEL9K_TERRAFORM_VERSION_SHOW_ON_UPGLOB='*.tf|*.tfvars|.terraform'

  # --- PROMPT CHAR ---
  typeset -g POWERLEVEL9K_PROMPT_CHAR_BACKGROUND=
  typeset -g POWERLEVEL9K_PROMPT_CHAR_OK_{VIINS,VICMD,VIVIS,VIOWR}_FOREGROUND=$color_green
  typeset -g POWERLEVEL9K_PROMPT_CHAR_ERROR_{VIINS,VICMD,VIVIS,VIOWR}_FOREGROUND=$color_red
  typeset -g POWERLEVEL9K_PROMPT_CHAR_{OK,ERROR}_VIINS_CONTENT_EXPANSION='❯'
  typeset -g POWERLEVEL9K_PROMPT_CHAR_LEFT_PROMPT_LAST_SEGMENT_END_SYMBOL=
  typeset -g POWERLEVEL9K_PROMPT_CHAR_LEFT_PROMPT_FIRST_SEGMENT_START_SYMBOL=

  # ===============================================================
  # 5. TECHNICAL SETTINGS
  # ===============================================================
  typeset -g POWERLEVEL9K_VCS_MAX_INDEX_SIZE_DIRTY=-1
  typeset -g POWERLEVEL9K_VCS_BACKENDS=(git)
  typeset -g POWERLEVEL9K_DISABLE_HOT_RELOAD=true

  (( ! $+functions[p10k] )) || p10k reload
}

typeset -g POWERLEVEL9K_CONFIG_FILE=${${(%):-%x}:a}
(( ${#p10k_config_opts} )) && setopt ${p10k_config_opts[@]}
'builtin' 'unset' 'p10k_config_opts'
