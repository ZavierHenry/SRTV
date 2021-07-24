#!/bin/bash

# Checks whether all example tweet files have a corresponding
# validation test in the validation test file

EXAMPLE_TWEET_DIRECTORY='srtv_tests/tweets'
VALIDATION_TEST_FILE='srtv_tests/Tests.fs'
VALIDATION_TEST_FUNCTION_NAME='``examples are valid``'

EXAMPLES=$(perl -pe 's/\r?\n/ /;' < $VALIDATION_TEST_FILE | 

    perl -ne "if ( /\[<Theory>\]((?:\s*\[<InlineData\(\"[^\"]+\.json\"\)>\])*)\s*member _+\.$VALIDATION_TEST_FUNCTION_NAME/ ) { print \$1; }" |
    perl -pe 's/\[<InlineData\("(.*?\.json)"\)>\]/$1/ge' 
)

FILES=$(find "$EXAMPLE_TWEET_DIRECTORY" -type f -name '*.json' | sed -e "s/$(echo $EXAMPLE_TWEET_DIRECTORY | sed 's/\//\\\//')\///")
BAD_EXAMPLES=$(for FILE in $FILES
    do 
    echo -e $EXAMPLES | 
    perl -ne "if ( index(\$_, '$FILE') == -1 ) { print '\n\t$FILE'; }"
    done
)

echo -e "Files without a validation test:$BAD_EXAMPLES"

if [ ! -z "$BAD_EXAMPLES" ]; then
    exit -1
fi

exit 0