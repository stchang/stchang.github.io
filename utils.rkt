#lang racket
(require scribble/base ; margin-note
         scribble/html-properties ; alt-tag
         scribble/core ; element
         racket/date)

(provide (all-defined-out))

(define (show-date)
  (parameterize ([date-display-format 'rfc2822])
    (margin-note (emph "Last updated:") (linebreak) (date->string (current-date)))))

;; disqus ---------------------------------------------------------------------

;<div id="disqus_thread"></div>
;    <script type="text/javascript">
;        /* * * CONFIGURATION VARIABLES: EDIT BEFORE PASTING INTO YOUR WEBPAGE * * */
;        var disqus_shortname = 'stchanggithub'; // required: replace example with your forum shortname
;
;        /* * * DON'T EDIT BELOW THIS LINE * * */
;        (function() {
;            var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
;            dsq.src = '//' + disqus_shortname + '.disqus.com/embed.js';
;            (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
;        })();
;    </script>
;    <noscript>Please enable JavaScript to view the <a href="http://disqus.com/?ref_noscript">comments powered by Disqus.</a></noscript>
;    <a href="http://disqus.com" class="dsq-brlink">comments powered by <span class="logo-disqus">Disqus</span></a>
    
(define DISQUS_SHORTNAME "stchanggithub")

(define (disqus post-id)
  (element
   (style #f (list (alt-tag "div")
                   (attributes
                    (list (cons 'id "disqus_thread")))))
   (element
    (style #f
     (list
      (script-property
       "text/javascript"
       (list (format "var disqus_shortname = '~a';\n" DISQUS_SHORTNAME)
             (format "var disqus_identifier = '~a';\n" post-id)
;             "var disqus_script = 'embed.js';\n"
             "(function () {\n"
             "var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;\n"
             "dsq.src = '//' + disqus_shortname + '.disqus.com/embed.js';\n"
             "(document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);\n"
             "})();\n"))))
    (list "Please enable JavaScript to view the comments powered by Disqus."))))
