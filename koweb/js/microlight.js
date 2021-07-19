/**
 * @fileoverview microlight - syntax highlightning library
 * @version 0.0.7
 *
 * @license MIT, see http://github.com/asvd/microlight
 * @copyright 2016 asvd <heliosframework@gmail.com>
 *
 * Code structure aims at minimizing the compressed library size
 */

export function microlight(el) {
    var _window       = window,
        _document     = document,
        appendChild   = 'appendChild',
        test          = 'test',
        // style and color templates
        textShadow    = ';text-shadow:',
        opacity       = 'opacity:.',
        _0px_0px      = ' 0px 0px ',
        _3px_0px_5    = '3px 0px 5',
        brace         = ')';

    var text  = el.textContent,
        pos   = 0,       // current position
        next1 = text[0], // next character
        chr   = 1,       // current character
        prev1,           // previous character
        prev2,           // the one before the previous
        token =          // current token content
        el.innerHTML = '',  // (and cleaning the node)
        
        // current token type:
        //  0: anything else (whitespaces / newlines)
        //  1: operator or brace
        //  2: closing braces
        //  3: (key)word
        //  4: quoted identifier {| |}
        //  5: multiline comment (; ;)
        tokenType = 0,

        node,

        // calculating the colors for the style templates
        colorArr = /(\d*\, \d*\, \d*)(, ([.\d]*))?/g.exec(
            _window.getComputedStyle(el).color
        ),
        pxColor = 'px rgba('+colorArr[1]+',',
        alpha = colorArr[3]||1;

    // running through characters and highlighting
    while (prev2 = prev1,
           prev1 = chr
    ) {
        chr = next1;
        next1=text[++pos];

        // checking if current token should be finalized
        if (!chr  || // end of content
            [ // finalize conditions for other token types
                // 0: whitespaces
                /\S/[test](chr),  // merged together
                // 1: operators
                1,                // consist of a single character
                // 2: braces
                1,                // consist of a single character
                // 3: (key)word
                !/[$\w]/[test](chr),
                // 4: quoted identifier
                prev2+prev1 == '|}',
                // 5: multiline comment
                prev2+prev1 == ';)'
            ][tokenType]
        ) {
            // appending the token to the result
            if (token) {
                // remapping token type into style
                // (some types are highlighted similarly)
                el[appendChild](
                    node = _document.createElement('span')
                ).setAttribute('style', [
                    // 0: not formatted
                    '',
                    // 1: keywords
                    textShadow + _0px_0px+9+pxColor + alpha * .7 + '),' +
                                 _0px_0px+2+pxColor + alpha * .4 + brace,
                    // 2: punctuation
                    opacity + 6 +
                    textShadow + _0px_0px+7+pxColor + alpha / 4 + '),' +
                                 _0px_0px+3+pxColor + alpha / 4 + brace,
                    // 3: comments
                    'font-style:italic;'+
                    opacity + 5 +
                    textShadow + _3px_0px_5+pxColor + alpha / 4 + '),-' +
                                 _3px_0px_5+pxColor + alpha / 4 + brace
                ][
                    // not formatted
                    !tokenType ? 0 :
                    // punctuation
                    tokenType < 3 ? 2 :
                    // comments
                    tokenType > 4 ? 3 :
                    // otherwise tokenType == 3, (key)word
                    // (1 if regexp matches, 0 otherwise)
                    + /^(thm|def|Type)$/[test](token)
                ]);

                node[appendChild](_document.createTextNode(token));
            }

            // initializing a new token
            token = '';

            // determining the new token type (going up the
            // list until matching a token type start
            // condition)
            tokenType = 6;
            while (![
                1,                   //  0: whitespace
                                     //  1: operator or braces
                /[\[(\-=>:.]/[test](chr),
                /[\])]/[test](chr),  //  2: closing brace
                /[$\w]/[test](chr),  //  3: (key)word
                chr+next1 == '{|',   //  4: quoted identifier
                chr+next1 == '(;'    //  5: multiline comment
            ][--tokenType]);
        }

        token += chr;
    }
}
