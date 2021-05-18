#lang racket

;; ============================================================
;; Modelos:

;; Elementos do mundo:
(struct verbo (sinonimos       ; lista de simbolos
              desc          ; string
              transitivo?)) ; boolean

(struct coisa (nome         ; simbolo
               [estado #:mutable] ; qualquer valor
               acoes))    ; lista de pares verbo--coisa

(struct local (desc         ; string
               [coisas #:mutable] ; lista de coisas
               acoes))    ; lista pares verbo--coisa

;; Tabelas mapeando nomes<->coisas para o save e o load
(define nomes (make-hash))
(define elementos (make-hash))

(define (armazenar-elemento! nome valor)
  (hash-set! nomes nome valor)
  (hash-set! elementos valor nome))

(define (nome->elemento nome) (hash-ref nomes nome #f))
(define (elemento->nome obj) (hash-ref elementos obj #f))

;; ============================================================
;; O Mundo:

;; Verbos ----------------------------------------
;; Declara todos os verbos que podem ser usados no jogo.
;; Cada verbo tem um nome canonico, um conjunto de sinonimos,
;; uma forma impressa, e um boolean indicando se ele é
;; transitivo ou nao

(define norte (verbo (list 'norte 'n) "vá para o norte" #f))
(armazenar-elemento! 'norte norte)

(define sul (verbo (list 'sul 's) "vá para o sul" #f))
(armazenar-elemento! 'sul sul)

(define leste (verbo (list 'leste 'l) "vá para o leste" #f))
(armazenar-elemento! 'leste leste)

(define oeste (verbo (list 'oeste 'o) "vá para o oeste" #f))
(armazenar-elemento! 'oeste oeste)

(define entrar (verbo (list 'entrar 'entre) "entrar" #f))
(armazenar-elemento! 'entrar entrar)

(define sair (verbo (list 'sair 'saia) "sair" #f))
(armazenar-elemento! 'sair sair)

(define voltar (verbo (list 'voltar 'retroceder) "voltar" #f))
(armazenar-elemento! 'voltar voltar)

(define lutar (verbo (list 'lutar 'enfrentar 'lute) "lutar" #f))
(armazenar-elemento! 'lutar lutar)

(define pegar (verbo (list 'pegar 'apanhar 'levar) "pegar" #t))
(armazenar-elemento! 'pegar pegar)

(define ler (verbo (list 'ler 'ver) "ler" #t))
(armazenar-elemento! 'ler ler)

(define abrir (verbo (list 'abrir 'destrancar) "abrir" #t))
(armazenar-elemento! 'abrir abrir)

(define quit (verbo (list 'quit 'exit) "quit" #f))
(armazenar-elemento! 'quit quit)

(define olhar (verbo (list 'olhar 'mostrar) "olhar" #f))
(armazenar-elemento! 'olhar olhar)

(define inventario (verbo (list 'inventario) "conferir inventario" #f))
(armazenar-elemento! 'inventario inventario)

(define ajuda (verbo (list 'ajuda 'socorro) (symbol->string 'ajuda) #f))
(armazenar-elemento! 'ajuda ajuda)

(define save (verbo (list 'save) (symbol->string 'save) #f))
(armazenar-elemento! 'save save)

(define load (verbo (list 'load) (symbol->string 'load) #f))
(armazenar-elemento! 'load load)

#|
;; Removido por Manoel Mendonca
(define all-verbs
  (list north south east west up down in out
        get put open close knock quit
        look inventory help save load))
|#


;; Added by Manoel Mendonca 25/03/2021
;; mesmo resultado de antes, porém muito mais seguro
(define todos-verbos (filter verbo? (hash-keys elementos)))


;; Ações Global ----------------------------------------
;; Lida com verbos que funcionam em qualquer local.

(define qualquerLocal-acoes
  (list
   (cons quit (lambda () (begin (printf "Tchau, até a próxima!\n") (exit))))
   (cons olhar (lambda () (mostrar-local-atual)))
   (cons inventario (lambda () (mostrar-inventario)))
   (cons save (lambda () (save-game)))
   (cons load (lambda () (load-game)))
   (cons ajuda (lambda () (mostrar-ajuda)))))

;; Coisas ----------------------------------------
;; Cada coisa cuida de um conjunto de verbos transitivos.


(define artefato1
  (coisa 'artefato1 
         #f 
         (list (cons pegar 
                    (lambda () 
                      (if (tem-coisa? artefato1)
                        "Você já possui esse artefato1."
                        (begin 
                          (tem-coisa! artefato1)
                          "Você agora tem esse artefato1.")))))))
(armazenar-elemento! 'artefato1 artefato1)

(define monstro1
  (coisa 'monstro1
         'vivo 
         (list (cons lutar 
                    (lambda () 
                      (if (and (eq? (coisa-state monstro1) 'vivo) (tem-coisa? artefato1))
                        (begin
                          (set-coisa-state! monstro1 'morto)
                          "Você conseguiu derrotar o monstro!")
                        (if (eq? (coisa-state monstro1) 'morto)
                            "Esse monstro já foi derrotado!"
                            "Você não possui o artefato certo para lutar contra esse monstro")))))))
(armazenar-elemento! 'monstro1 monstro1)

(define dica-sala1
  (coisa 'dica-sala1
         #f
         (list (cons ler 
                    (lambda () 
                        "Use os dedos para jogar.")))))
(armazenar-elemento! 'dica-sala1 dica-sala1)

(define portal1
  (coisa 'portal1
         #f
         (list (cons usar 
                    (lambda () sala1))
               (cons entrar 
                    (lambda () sala1)))))
(armazenar-elemento! 'portal-sala1 portal-sala1)

(define tesouro
  (coisa 'tesouro
         #f
         (list
          (cons pegar 
                (lambda ()
                  (begin
                    (tem-coisa! tesouro)
                    "Você ganhou!"))))))
(armazenar-elemento! 'tesouro tesouro)

#| exemplo de coisa:
(define door
  (coisa 'door
         #f
         (list
          (cons open 
                (lambda ()
                  (if (tem-coisa? key)
                      (begin
                        (set-thing-state! door 'open)
                        "The door is now unlocked and open.")
                      "The door is locked.")))
          (cons close 
                (lambda ()
                  (begin
                    (set-thing-state! door #f)
                    "The door is now closed.")))
          (cons knock 
                (lambda ()
                  "No one is home.")))))
(record-element! 'door door)

(define key
  (coisa 'key
         #f
         (list
          (cons get 
                (lambda ()
                  (if (tem-coisa? key)
                      "You already have the key."
                      (begin
                        (take-thing! key)
                        "You now have the key."))))
          (cons put 
                (lambda ()
                  (if (tem-coisa? key)
                      (begin
                        (drop-thing! key)
                        "You have dropped the key.")
                      "You don't have the key."))))))
(record-element! 'key key)

(define trophy
  (coisa 'trophy
         #f
         (list
          (cons get 
                (lambda ()
                  (begin
                    (take-thing! trophy)
                    "You win!"))))))
(record-element! 'trophy trophy)
|#

;; Places ----------------------------------------
;; Each place handles a set of non-transitive verbs.

(define meadow
  (place
   "You're standing in a meadow. There is a house to the north."
   (list)
   (list
    (cons north 
          (lambda () house-front))
    (cons south 
          (lambda () desert)))))
(record-element! 'meadow meadow)

(define house-front
  (place
   "You are standing in front of a house."
   (list door)
   (list
    (cons in 
          (lambda ()
            (if (eq? (thing-state door) 'open)
                room
                "The door is not open.")))
    (cons south (lambda () meadow)))))
(record-element! 'house-front house-front)

(define desert
  (place
   "You're in a desert. There is nothing for miles around."
   (list cactus key)
   (list
    (cons north (if (eq? (thing-state monstro1) 'morto)
                    (lambda () meadow)
                    "O monstro está bloqueando o caminho!"))
    (cons south (lambda () desert))
    (cons east (lambda () desert))
    (cons west (lambda () desert)))))
(record-element! 'desert desert)

(define room
  (place
   "You're in the house."
   (list trophy)
   (list (cons out (lambda () house-front)))))
(record-element! 'room room)

;; ============================================================
;; Estado do Jogo

;; Coisas carregadas pelo jogador:
(define pertences null) ; lista de coisas

;; Localização Atual:
(define local-atual meadow) ; local

;; Localização Anterior:
(define local-anterior meadow) ; local

;; Funções para serem usadas por respostas de verbos
(define (tem-coisa? t)
  (memq t pertences))

(define (pegar-coisa! t) 
  (set-local-coisas! local-atual
                     (remq t (local-coisas local-atual)))
  (set! pertences (cons t pertences)))

(define (soltar-coisa! t) 
  (set-local-coisas! local-atual
                     (cons t (local-coisas local-atual)))
  (set! pertences (remq t pertences)))


;; ============================================================
;; Execução do jogo

;; Inicializa e começa
;; Mostra o local atual do jogador, então recebe um comando:
(define (execute-local)
  (mostrar-local-atual) ; mostra local atual
  (processe-verbo))           ; executa comando

;; Mostra o local atual
(define (mostrar-local-atual)
  (printf "~a\n" (local-desc local-atual)) ; imprime o local
  (for-each (lambda (coisa)      ; imprime as coisas do local
              (printf "Tem um/a ~a here.\n" (coisa-nome coisa)))
            (local-coisas local-atual)))

;; Loop principal
;; Pega e trata um comando:
(define (processe-verbo)
  (printf "> ")             ; imprime o prompt
  (flush-output)
  (let* ([line (read-line)] ; lê comando
         [input (if (eof-object? line)  ; vê se foi um comando de fim de arquivo
                    '(quit)             ; se sim, sai
                    (let ([port (open-input-string line)]) ; se não, coloca palavras
                      (for/list ([v (in-port read port)]) v)))])  ; em "input"
    (if (and (list? input)            ; se input é lista,
             (andmap symbol? input)   ; tem só símbolos,
             (<= 1 (length input) 2)) ; e tem um ou dois símbolos, é um comando correto
        (let ([cmd (car input)]) ;; o comando principal, verbo, é o começo da lista
            (let ([response ;; monta resposta para verbos
                   (cond
                    [(= 2 (length input))
                     (trata-verbo-transitivo cmd (cadr input))] ;; transitivos
                    [(= 1 (length input))
                     (trata-verbo-intransitivo cmd)])])         ;; intransitivos
              (let ([result (response)]) ;; resposta é uma função, execute-a
                (cond
                 [(local? result) ;; se o resultado for um local
                  (set! local-atual result) ;; ele passa a ser o novo local
                  (execute-local)]   ;; faça o processamento do novo local, loop
                 [(string? result) ; se a resposta for uma string
                  (printf "~a\n" result)  ; imprima a resposta
                  (processe-verbo)]    ; volte a processar outro comando, loop
                 [else (processe-verbo)])))) ; caso contrário, outro comando, loop
          (begin ; comando incorreto
            (printf "Não entendi o que quis dizer.\n")
            (processe-verbo)))))


;; Trata um comando de verbo-intransitivo:
;; retorna função para processar verbo intrasitivo

(define (trata-verbo-intransitivo cmd)
  (or
   ; considerando o local, retorna a ação associada ao verbo
   (encontra-verbo cmd (local-acoes local-atual))
   ; se não achou no local, considerando o jogo todo, retorna a ação associada ao verbo
   (encontra-verbo cmd qualquerLocal-acoes)
   ; se não achou no local ou no geral, mas o verbo existe
   ; retorna uma função que dá uma mensagem de erro em contexto
   (usando-verbo  ; procura o verbo, obtem info descritiva, e retorna a função abaixo
    cmd todos-verbos
    (lambda (verbo)
      (lambda () ; função retornada por usando-verbo, mensagem de erro em contexto
        (if (verbo-transitivo? verbo)
            (format "~a o quê?" (string-titlecase (verbo-desc verbo)))
            (format "Não pode ~a aqui." (verbo-desc verbo))))))
   (lambda () ; não achou o verbo no jogo
     (format "Eu não sei como fazer ~a." cmd))))

;; Trata um comando de verbo transitivo:
(define (trata-verbo-transitivo cmd obj)
  (or (usando-verbo ; produz ação para verbo, retorna falso se não achar verbo no jogo
       cmd todos-verbos
       (lambda (verbo) ; função retornada
         (and ; retorna falso se alguma destas coisas for falsa
          (verbo-transitivo? verbo) ; verbo é transitivo? - funcão criada por struct 
          (cond
           [(ormap (lambda (coisa) ; verifica se o objeto nomeado existe em contexto
                     (and (eq? (coisa-nome coisa) obj)
                          coisa))
                   ; na lista das coisas do local e das coisas que tenho (pertences)
                   (append (local-coisas local-atual) 
                           pertences))
            => (lambda (coisa) ; se existe, aplica esta função sobre a coisa/coisa
                 (or (encontra-verbo cmd (coisa-acoes coisa)) ; retorna acão que se aplica a coisa
                     (lambda () ; se ação não encontrada, indica que não há ação
                       (format "Não sei como fazer ~a ~a."
                               (verbo-desc verbo) obj))))]
           [else ; se objeto não existe
            (lambda ()
              (format "Não tem um ~a aqui para ~a." obj 
                      (verbo-desc verbo)))]))))
      (lambda ()  ; se não achou o verbo
        (format "Eu não sei como fazer ~a ~a." cmd obj))))

;; Mostra o que o jogador está carregando:
(define (mostrar-inventario)
  (printf "Você tem")
  (if (null? pertences)
      (printf " nenhum item.")
      (for-each (lambda (coisa) ; aplica esta função a cada coisa da lista
                  (printf "\n  um ~a" (coisa-nome coisa)))
                pertences))
  (printf "\n"))

;; Procura por um equivalência de comando numa lista de pares verbo--resposta,
;; e retorna a resposta se uma equivalência é encontrada:
(define (encontra-verbo cmd acoes)
  (ormap (lambda (a)
           (and (memq cmd (verbo-sinonimos (car a)))
                (cdr a)))
         acoes))

;; Procura por um comando na lista de verbos e,
;; aplica `sucess-k' para o verbo se algum é encontrado:
(define (usando-verbo cmd verbos success-k)
  (ormap (lambda (vrb)
           (and (memq cmd (verbo-sinonimos vrb))
                (success-k vrb)))
         verbos))

;; Imprimir informação de ajuda:
(define (mostrar-ajuda)
  (printf "Use `olhar' para olhar em volta.\n")
  (printf "Use `inventário' para ver o que você possui.\n")
  (printf "Use `save' ou `load' para salvar ou restaurar seu jogo.\n")
  (printf "There are some other verbs, and you can name a thing after some verbs.\n"))

;; ============================================================
;; Save and load

;; Prompt the user for a filename and apply `proc' to it,
;; catching errors to report a reasonably nice message:
(define (with-filename proc)
  (printf "File name: ")
  (flush-output)
  (let ([v (read-line)])
    (unless (eof-object? v)
      (with-handlers ([exn? (lambda (exn)
                              (printf "~a\n" (exn-message exn)))])
        (unless (path-string? v)
          (raise-user-error "bad filename"))
        (proc v)))))

;; Save the current game state:
(define (save-game)
  (with-filename
   (lambda (v)
     (with-output-to-file v
       (lambda ()
         (write
          (list
           (map elemento->nome pertences)
           (elemento->nome local-atual)
           (hash-map nomes
                     (lambda (k v)
                       (cons k
                             (cond
                              [(local? v) (map elemento->nome (local-coisas v))]
                              [(coisa? v) (coisa-estado v)]
                              [else #f])))))))))))

;; Restore a game state:
(define (load-game)
  (with-filename
   (lambda (v)
     (let ([v (with-input-from-file v read)])
       (set! pertences (map nome->elemento (car v)))
       (set! local-atual (nome->elemento (cadr v)))
       (for-each
        (lambda (p)
          (let ([v (nome->elemento (car p))]
                [estado (cdr p)])
            (cond
             [(local? v) (set-local-coisas! v (map nome->elemento estado))]
             [(coisa? v) (set-coisa-estado! v estado)])))
        (caddr v))))))

;; ============================================================
;; Começar!

(execute-local)