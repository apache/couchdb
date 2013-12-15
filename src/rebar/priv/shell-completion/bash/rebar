# bash completion for rebar

_rebar()
{
    local cur prev sopts lopts cmdsnvars
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    sopts="-h -c -v -V -f -D -j -C -p -k"
    lopts="--help \
        --commands \
        --verbose \
        --force \
        --jobs \
        --config \
        --profile \
        --keep-going \
        --version"
    cmdsnvars="check-deps \
        clean \
        compile \
        create \
        create-app \
        create-node \
        ct \
        doc \
        delete-deps \
        escriptize \
        eunit \
        get-deps \
        generate \
        generate-appups \
        generate-upgrade \
        help \
        list-deps \
        list-templates \
        qc \
        update-deps \
        version \
        xref \
        overlay \
        apps= \
        case= \
        dump_spec=1 \
        force=1 \
        jobs= \
        suites= \
        verbose=1 \
        appid= \
        overlay_vars= \
        previous_release= \
        nodeid= \
        root_dir= \
        skip_deps=true \
        skip_apps= \
        target_dir= \
        template= \
        template_dir= \
        tests="

    if [[ ${cur} == --* ]] ; then
        COMPREPLY=( $(compgen -W "${lopts}" -- ${cur}) )
    elif [[ ${cur} == -* ]] ; then
        COMPREPLY=( $(compgen -W "${sopts}" -- ${cur}) )
    else
        COMPREPLY=( $(compgen -W "${cmdsnvars}" -- ${cur}) )
    fi

    if [ -n "$COMPREPLY" ] ; then
        # append space if matched
        COMPREPLY="${COMPREPLY} "
        # remove trailing space after equal sign
        COMPREPLY=${COMPREPLY/%= /=}
    fi
    return 0
}
complete -o nospace -F _rebar rebar

# Local variables:
# mode: shell-script
# sh-basic-offset: 4
# sh-indent-comment: t
# indent-tabs-mode: nil
# End:
# ex: ts=4 sw=4 et filetype=sh
