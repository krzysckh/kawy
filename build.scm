(import
 (owl toplevel)
 (owl metric)
 (prefix (owl sys) sys/)
 (prefix (robusta encoding html) html/))

(define-syntax opinion
  (syntax-rules (42 on is)
    ((_ on name_ . rest)
     (_ 42 (put empty 'on name_) . rest))
    ((_ 42 ff key is (value ...) . rest)
     (_ 42 (put ff 'key '(value ...)) . rest))
    ((_ 42 ff key is value . rest)
     (_ 42 (put ff 'key '(value)) . rest))
    ((_ 42 ff)
     ff)))

(define *brews*
  (pipe empty
    (put 'drip     "przelewowo")
    (put 'espresso "w ekspresie kolbowym")
    (put 'french   "we french pressie")
    (put 'ew       "zalewaną")
    (put 'moka     "w kawiarce")
    (put 'briki    "w tygielku")
    ))

(define *opinions*
  (list
   (opinion on "HAYB SIE PRZELEWA Kwiat"
     tint   is "#f0f"
     rating is 10
     image  is "HAYB.sieprzelewa.kwiat.jpg"
     link   is "https://haybcoffee.pl/produkt/sie-przelewa-kwiat/"
     brew   is drip
     text   is ("genialna. "
                "zdecydowany faworyt do wiosennego przelewu. "
                "w smaku nierzadko herbaciana. "
                "bardzo delikatna. przy parzeniu z większej ilości kawy zupełnie nie kwaśna, tylko lekuteńko gorzkawa. "
                "zdecydowanie godna polecenia."))
   (opinion on "HAYB SIE PRZELEWA Owoc"
     tint   is "#ec6b20"
     rating is 6
     image  is "HAYB.sieprzelewa.owoc.jpg"
     link   is "https://haybcoffee.pl/produkt/sie-przelewa-owoc/"
     brew   is drip
     text   is ("spoko, przyjemna, codzienna. lekko kwaskowa. "
                "niestety cenowo taka sama jak kwiat, a — moim zdaniem — ma sporo uboższy smak, co odejmuje jej parę punktów"))
   (opinion on "BONORA Καφές Ελληνικός"
     tint   is "#919191"
     rating is 7
     image  is "BONORA.ellinikos.kafes.jpg"
     link   is "https://www.sklavenitis.gr/eidi-proinoy-rofimata/kafedes-rofimata-afepsimata/kafedes-ellinikoi/bonora-ellinikos-kafes-487gr-240959/"
     brew   is briki
     text   is ("chyba moja ulubiona wśród kaw po grecku. "
                "wprost z greckiego supermarketu. dużo lepsza od loumidis papagalos kupionej w kuchniach świata. "
                "smakuje najlepiej zimą, z odrobiną kardamonu."))
   (opinion on "JA-WA kolubmia bezkofeinowa"
     tint   is "gold"
     rating is 7
     image  is "JAWA.decaf.kolumbia.jpg"
     link   is "https://www.palarniakawyjawa.pl/kawa-bezkofeinowa-kolumbia/"
     brew   is (moka espresso)
     text   is ("naprawdę spoko. bezkofeinowość w smaku zdecydowanie niewyczuwalna. "
                "kwaskowa w przyjemny sposób. palona ciemno."))
   (opinion on "DIY Indie Cherry AA"
     tint   is "#D9001D"
     rating is 3
     image  is ("DIY.cafemia.indiecherry1.jpg"
                "DIY.cafemia.indiecherry2.jpg"
                "DIY.cafemia.indiecherry3.jpg")
     link   is "https://cafemia.pl/pl/p/INDIA-CHERRY-AA/82"
     brew   is (moka espresso drip ew)
     text   is ("zabawa przednia — wypalałem ją na kilka sposobów (m.in. na ognisku). "
                "niestety wyniki (zupełnie nie zaskakująco) raczej słabe. "
                "sporo wypaliłem zbyt ciemno, sporo zbyt jasno. bez sprzętu ciężko jest uzyskać jednolite wypalenie. "
                "no nie była ona najlepsza ;). ")
     )
   ))

(define (start-gensym)
  (thread
   'gensym
   (let loop ((n 1))
     (lets ((who v (next-mail)))
       (mail who (str "g" n))
       (loop (+ n 1))))))

(define (gensym)
  (interact 'gensym '_))

(define unfuck (string->regex "s/\"/'/g"))

(define *polish-is-hard*
  '("jedno ziarenko"
    "dwa ziarenka"
    "trzy ziarenka"
    "cztery ziarenka"
    "pięć ziarenek"
    "sześć ziarenek"
    "siedem ziarenek"
    "osiem ziarenek"
    "dziewięć ziarenek"
    "dziesięć ziarenek"))

(define (make-brews l)
  (let ((ss (filter self (map (λ (x) (get *brews* x #f)) l))))
    (when (< (len ss) 1)
      (error "no brews in " l))
    (let loop ((ss ss))
      (cond
       ((eq? (len ss) 1)
        (str (car ss) "."))
       ((eq? (len ss) 2)
        (str (car ss) " i " (cadr ss) "."))
       (else
        (str (car ss) ", " (loop (cdr ss))))))))

(define (dither! filename)
  (let ((d-filename (str filename ".dithered.gif")))
    (when (not (sys/file? d-filename))
      (system
       `("convert" ,filename
         "-resize" "640"
         "-dither" "FloydSteinberg"
         "-remap" "netscape:"
         "-colors" "8"
         ,d-filename))
      (let ((siz1 (len (file->list filename)))
            (siz2 (len (file->list d-filename))))
        (format stdout "[dither!] Saved ~a by dithering ~a~%"
                (format-number-base2 (- siz1 siz2))
                filename)))))

(define (make-images l)
  (map
   (λ (x)
     (let* ((g (gensym))
            (local-file (str "public/res/img/" x))
            (fsize (format-number-base2 (len (or (file->list local-file) ())))))
       (dither! local-file)
       `((img (id . ,g) (src . ,(str "res/img/" x ".dithered.gif")))
         ((span (onclick . ,(format #f "get_full(~a, ~a)"
                                    (unfuck (str* g))
                                    (unfuck (str* (str "res/img/" x))))))
          ,(format #f "[zdjęcie zditherowane. kliknij by pobrać całość (~a)]" fsize)))))
   l))

(define (make-box id opinion)
  `((div (class . "coffee-box") (id . ,id))
    ((div (class . "title") (style . ,(str "background: " (car (get opinion 'tint "black")))))
     (h2 ,(get opinion 'on "oops!")))
    ((div (class . "image"))
     ,@(make-images (get opinion 'image ())))
    (p ,(fold str "" (get opinion 'text "oops!")))
    (p "przygotowywałem ją " ,(make-brews (get opinion 'brew #n)))
    (div
     ((span (class . "rating"))
      (span "ocena: ")
      ((span (class . "beans")) ,@(make-list
                                   (car (get opinion 'rating 5))
                                   '((img (class . "bean") (src . "res/img/bean.png")))))
      (span ,(str " (słownie: " (lref *polish-is-hard* (- (car (get opinion 'rating 5)) 1)) " na dziesięć)" ))))
    ,@(if-lets ((l (car (get opinion 'link '(#f)))))
        `((div (span "można kupić ją " ((a (href . ,(car (get opinion 'link #f)))) "tutaj"))))
        ())
    ))

(define (map2 f a b)
  (if (null? a)
      ()
      (cons (f (car a) (car b)) (map2 f (cdr a) (cdr b)))))

(λ (_)
  (start-gensym)
  (let ((f (open-output-file "public/index.html"))
        (ids (map (λ (x) (x)) (make-list (length *opinions*) gensym))))
    (write-bytes
     f
     (string->bytes
      (html/encode
       `(html
         (head
          (title "kawy : )")
          ((meta (name . "viewport") (content . "width=device-width, initial-scale=1.0")))
          ((link (rel . "stylesheet") (href . "style.css")))
          ((meta (charset . "utf-8"))))
         (body
          (h1 "uwaga!")
          (p "przedstawione niżej są opinie, nie fakty."
             "to raczej pamiętnik, żurnal kaw wypitych z paroma wyrazami opisującymi co o nich zapamiętałem."
             "jeśli nie zgadzasz się z którąś z opinii, pamiętaj by napisać maila o tym co myślisz o mnie "
             "personalnie — jako człowieku — na adres " (b "kpm+kawa@krzysckh.org") " tytułem "
             (b "„nie masz pojęcia co robisz”")
             " — na takie odpisuję najszybciej." (br) "— kpm")
          (br)
          (p "spis \"treści\"")
          (ul
           ,@(map2 (λ (id op) `(li ((a (href . ,(str "#" id))) ,(get op 'on)))) ids *opinions*))
          ,@(map2 make-box ids *opinions*)
          ((script (src . "app.js"))))))))
    (close-port f))
  0)
