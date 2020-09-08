source /dev/stdin <<< "$(curl -s --retry 3 https://lang-common.s3.amazonaws.com/buildpack-stdlib/latest/stdlib.sh)"
