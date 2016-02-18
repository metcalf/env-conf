#!/bin/sh

CMDLINE=''
DEFINITELY_USE_BUNDLE=unclear

case $* in
    *$HOME/stripe/pay-server/.rubocop.yml*)
        echo "Running with the prison guard installed..."
        CMDLINE="$CMDLINE -r ~/stripe/pay-server/dev/lib/prison_guard.rb"
        DEFINITELY_USE_BUNDLE=true
        ;;
    *$HOME/stripe/answers/.rubocop.yml*)
        DEFINITELY_USE_BUNDLE=false
        ;;
    *$HOME/stripe/space-commander/.rubocop.yml*)
        DEFINITELY_USE_BUNDLE=true
        ;;
esac

case "$DEFINITELY_USE_BUNDLE" in
    false)
        ~/.rbenv/shims/rubocop $CMDLINE "$@"
    ;;
    unclear)
        if ! bundle check ; then
            echo "Bundle check not successful, rubocop will be unhappy."
            exit 0
        fi

        if bundle exec ruby -e 'exit(Gem.loaded_specs.has_key?("rubocop"))' ; then
            bundle exec rubocop $CMDLINE "$@"
        else
            ~/.rbenv/shims/rubocop $CMDLINE "$@"
        fi
        ;;
    true)
        bundle exec rubocop $CMDLINE "$@"
        ;;
esac
