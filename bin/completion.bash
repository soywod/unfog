#!/bin/bash

gen_comp() {
  COMPREPLY=($(compgen -W "$1" -- "$2"))
}

unfog_completions() {
  comp_words_len="${#COMP_WORDS[@]}"
  ids="$(unfog list ids)"
  tags="$(unfog list tags)"

  case $comp_words_len in
    2)
      gen_comp "add info list edit set toggle done undone delete remove context worktime status upgrade help" "${COMP_WORDS[1]}"
      ;;

    3)
      case "${COMP_WORDS[1]}" in
        info | edit | set | toggle | done | undone | delete | remove | status)
          gen_comp "$ids" "${COMP_WORDS[2]}"
          ;;

        list)
          gen_comp "ids tags" "${COMP_WORDS[2]}"
          ;;

        *)
          gen_comp "$tags" "${COMP_WORDS[2]}"
      esac
      ;;

    *)
      gen_comp "$tags" "${COMP_WORDS[$comp_words_len - 1]}"
  esac
}

complete -F unfog_completions unfog
complete -F unfog_completions u
