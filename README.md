A parser for Apache Http Server 2.2 configuration files, just for fun!

The intent is to be able to parse config files and create a visible map/graph of the configuration.
    
Following is what the grammar for an apache httpd.conf (and other config files) should be (this is taken from an old book on configuring apache 2.0):

    config              ::= directive*
    directive           ::= section-directive | simple-directive
    section-directive   ::= section-open configuration section-closed
    section-open        ::= "<" directive-name directive-argument* ">"
    section-close       ::= "</" directive-name ">"
    simple-directive    ::= directive-name directive-argument*

    directive-name      ::= ...
        (see http://httpd.apache.org/docs/2.2/mod/directives.html for a list
            of possible directives and their arguments)
            
    directive-arg       ::= ...
        (see http://httpd.apache.org/docs/2.2/mod/directives.html for a list
            of possible directives and their arguments)
        
This is a work in progress.
