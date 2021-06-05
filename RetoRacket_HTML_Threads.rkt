#lang racket

;Ignacio Joaquin Moral
;Alfredo JeongHyun Park
;Valter Kuhne

(require racket/trace)

(define (get-html-output input-string)
  (let
      ([matches (regexp-match #px"([\\w-]+)(\\.\\w{1,4})" input-string)])
    (string-append (cadr matches) ".html")))

(define (generate-span string type)
  (format "<span class = '~a' >~a</span>" type string))

(define (convert-html in-file-path)
  (define file (file->string in-file-path))
  (let loop
    ([result ""][file file])
    (if (non-empty-string? file)
        (let
            ([tokens
              (cond
                ;First, check if it's a special separation value, like {, }, [, ], and commas
                [(regexp-match #px"^[:[\\]{},]" file) (list (car (regexp-match #px"^[\\{\\}\\[\\]\\:\\,]" file)) "separation")]
                ;Then, check if the string you found is a key, or the value before the ':'
                [(regexp-match #px"^\"[a-zA-Z\\.\\:\\/\\?\\!\\,\\;\\'\\&\\*\\-\\+\\@\\=\\0-9\\s]*?\"\\:[\\s]?" file) (list (car (regexp-match #px"^\"[a-zA-Z\\.\\:\\/\\?\\!\\,\\;\\'\\&\\*\\-\\+\\@\\=\\0-9\\s]*?\"\\:[\\s]?" file)) "key")]
                ;Then, check if it's a string of a key, or a value
                [(regexp-match #px"^\"[a-zA-Z\\.\\:\\/\\?\\!\\,\\;\\'\\&\\*\\-\\+\\@\\=\\0-9\\s]*?\"" file) (list (car (regexp-match #px"^\"[a-zA-Z\\.\\:\\/\\?\\!\\,\\;\\'\\&\\*\\-\\+\\@\\=\\0-9\\s]*?\"" file)) "value")]
                ;Then, check if it's a number
                [(regexp-match #px"^(-)?[\\d\\.]+([eE][-+]?[\\d]*)?" file) (list (car (regexp-match #px"^(-)?[\\d\\.]+([eE][-+]?[\\d]*)?" file)) "number")]
                ;Then, check if it's a boolean, like null
                [(regexp-match #px"^true|false|True|False|null|NULL" file) (list (car (regexp-match #px"^true|false|True|False|null|NULL" file)) "boolean")]
                ;Finally, check if it's empty space
                [(regexp-match #px"^(\\s+)" file) (list (car (regexp-match #px"^(\\s+)" file)) "empty_space")])])
          (loop (string-append result (generate-span (car tokens) (cadr tokens))) (substring file (string-length (car tokens)))))
        result
        )))

(define (write-file out-file-path data)
  (call-with-output-file out-file-path
    #:exists 'truncate
    (lambda (out)
      (let loop
        ([lst data])
        (cond
          [(not (empty? lst))
           (displayln (car lst) out)
           (loop (cdr lst))])))))

(define (main in-file-path)
  (define result (list (format (file->string "regexpReto.html") (convert-html in-file-path))))
  (write-file (get-html-output in-file-path) result))


(define (make-future name in-file-path number)
  (define direcList (directory-list (string->path in-file-path)))
  (thread (lambda ()
  (let loop
    ([n 0] [list direcList] [result ""])
    (cond
      [(< n number)
                 (define result (list (format (file->string "regexpReto.html") (convert-html (car direcList)))))
                 (loop (add1 n) (cdr direcList) (string-append result result))]
      [else result])))))
  


(define (thread in-file-path number)
 (define futures (map make-future (range number) in-file-path
                       (make-list number in-file-path)))
  (define result (map touch futures))
  (write-file (get-html-output in-file-path) result))
