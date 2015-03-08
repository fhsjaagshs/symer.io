### jQuery is not necessary, and window.location.href(...) will best simulate an HTTP redirect.

It is better than using window.location.href =, because replace() does not put the originating page in the session history, meaning the user won't get stuck in a never-ending back-button fiasco. If you want to simulate someone clicking on a link, use location.href. If you want to simulate an HTTP redirect, use location.replace.

#### TODO: background around code.

For example:

    // similar behavior as an HTTP redirect
    window.location.replace("http://stackoverflow.com")