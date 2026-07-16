# shellcheck shell=bash
# Shows project name

run_segment() {
  eval "$(tmux show-environment -s TMUX_POWERLINE_SEG_PROJECT_STRIP_PATTERN)"

  if [ -z "$TMUX_POWERLINE_SEG_PROJECT_STRIP_PATTERN" ]; then
    echo '#{s|-[0-9]*$||:session_name}'

  else
    echo "#{s|-[0-9]*$||;s|${TMUX_POWERLINE_SEG_PROJECT_STRIP_PATTERN}||:session_name}"

  fi

  return 0
}
