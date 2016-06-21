function has_preprocessing() {
    output=`find "$d" -iname '*.f*' -exec egrep "^# *(if|def|inc)" \{\} \;`
    if [ -z "$output" ]; then
        echo 0
    else
        echo 1
    fi
}

function has_include() { # FIXME, someday
    echo 0
}

for d in "$@"; do
    if [ -d "$d" -a `has_preprocessing "$d"` == 0 -a `has_include "$d"` == 0 ]; then
        echo $d
    fi
done
