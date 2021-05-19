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

; TODO: ADICIONAR A COR DO CORREDOR
(struct local (desc         ; string
               cor          ; cor do corredor
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

(define usar (verbo (list 'usar 'use) "usar" #f))
(armazenar-elemento! 'usar usar)

(define entrar (verbo (list 'entrar 'entre) "entrar" #f))
(armazenar-elemento! 'entrar entrar)

(define sair (verbo (list 'sair 'saia) "sair" #f))
(armazenar-elemento! 'sair sair)

(define teleporte (verbo (list 'teleporte 't) "usar o teleporte" #f))
(armazenar-elemento! 'teleporte teleporte)

(define voltar (verbo (list 'voltar 'retroceder) "voltar" #f))
(armazenar-elemento! 'voltar voltar)

(define lutar (verbo (list 'lutar 'enfrentar 'lute) "lutar" #t))
(armazenar-elemento! 'lutar lutar)

(define pegar (verbo (list 'pegar 'apanhar 'levar) "pegar" #t))
(armazenar-elemento! 'pegar pegar)

(define ler (verbo (list 'ler 'ver) "ler" #t))
(armazenar-elemento! 'ler ler)

(define abrir (verbo (list 'abrir 'destrancar) "abrir" #t))
(armazenar-elemento! 'abrir abrir)

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
   (cons sair (lambda () (begin (printf "Tchau, até a próxima!\n") (exit))))
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
                          (pegar-coisa! artefato1)
                          "Você adquiriu o artefato1!")))))))
(armazenar-elemento! 'artefato1 artefato1)

(define monstroSala3
  (coisa 'minotauro
         #t 
         (list (cons lutar 
                    (lambda () 
                      (if (and (eq? (coisa-estado monstroSala3) #t) (tem-coisa? artefato1))
                        (begin
                          (set-coisa-estado! monstroSala3 #f)
                          (set-coisa-estado! artefato1 #f)
                          "Você conseguiu derrotar o monstro!")
                        (if (eq? (coisa-estado monstroSala3) #f)
                            "Esse monstro já foi derrotado!"
                            "Você não possui o artefato certo para lutar contra esse monstro")))))))
(armazenar-elemento! 'monstroSala3 monstroSala3)

(define monstroSala4
  (coisa 'centauro
         #t 
         (list (cons lutar 
                    (lambda () 
                      (if (and (eq? (coisa-estado monstroSala4) #t) (tem-coisa? artefato1))
                        (begin
                          (set-coisa-estado! monstroSala4 #f)
                          (set-coisa-estado! artefato1 #f)
                          "Você conseguiu derrotar o monstro!")
                        (if (eq? (coisa-estado monstroSala4) #f)
                            "Esse monstro já foi derrotado!"
                            "Você não possui o artefato certo para lutar contra esse monstro")))))))
(armazenar-elemento! 'monstroSala4 monstroSala4)

(define monstroSala10
  (coisa 'kraken
         #t 
         (list (cons lutar 
                    (lambda () 
                      (if (and (eq? (coisa-estado monstroSala10) #t) (tem-coisa? artefato1))
                        (begin
                          (set-coisa-estado! monstroSala10 #f)
                          (set-coisa-estado! artefato1 #f)
                          "Você conseguiu derrotar o monstro!")
                        (if (eq? (coisa-estado monstroSala10) #f)
                            "Esse monstro já foi derrotado!"
                            "Você não possui o artefato certo para lutar contra esse monstro")))))))
(armazenar-elemento! 'monstroSala10 monstroSala10)

(define monstroSala22
  (coisa 'harpia
         #t 
         (list (cons lutar 
                    (lambda () 
                      (if (and (eq? (coisa-estado monstroSala22) #t) (tem-coisa? artefato1))
                        (begin
                          (set-coisa-estado! monstroSala22 #f)
                          (set-coisa-estado! artefato1 #f)
                          "Você conseguiu derrotar o monstro!")
                        (if (eq? (coisa-estado monstroSala22) #f)
                            "Esse monstro já foi derrotado!"
                            "Você não possui o artefato certo para lutar contra esse monstro")))))))
(armazenar-elemento! 'monstroSala22 monstroSala22)

(define monstroSala25
  (coisa 'goblin
         #t 
         (list (cons lutar 
                    (lambda () 
                      (if (and (eq? (coisa-estado monstroSala25) #t) (tem-coisa? artefato1))
                        (begin
                          (set-coisa-estado! monstroSala25 #f)
                          (set-coisa-estado! artefato1 #f)
                          "Você conseguiu derrotar o monstro!")
                        (if (eq? (coisa-estado monstroSala25) #f)
                            "Esse monstro já foi derrotado!"
                            "Você não possui o artefato certo para lutar contra esse monstro")))))))
(armazenar-elemento! 'monstroSala25 monstroSala25)

(define monstroSala34
  (coisa 'troll
         #t 
         (list (cons lutar 
                    (lambda () 
                      (if (and (eq? (coisa-estado monstroSala34) #t) (tem-coisa? artefato1))
                        (begin
                          (set-coisa-estado! monstroSala34 #f)
                          (set-coisa-estado! artefato1 #f)
                          "Você conseguiu derrotar o monstro!")
                        (if (eq? (coisa-estado monstroSala34) #f)
                            "Esse monstro já foi derrotado!"
                            "Você não possui o artefato certo para lutar contra esse monstro")))))))
(armazenar-elemento! 'monstroSala34 monstroSala34)

(define dica
  (coisa 'dica
         #f
         (list (cons ler 
                    (lambda () 
                        "Use os dedos para jogar.")))))
(armazenar-elemento! 'dica dica)

(define portalSala9
  (coisa 'portalAlfa
         #f
         (list (cons usar
                     (lambda () sala31))
               (cons entrar 
                    (lambda () sala31))
               (cons teleporte
                     (lambda () sala31)))))
(armazenar-elemento! 'portalSala9 portalSala9)

(define portalSala16
  (coisa 'portalBeta
         #f
         (list (cons usar
                     (lambda () sala27))
               (cons entrar 
                    (lambda () sala27))
               (cons teleporte
                     (lambda () sala27)))))
(armazenar-elemento! 'portalSala16 portalSala16)

(define portalSala27
  (coisa 'portalAlfa
         #f
         (list (cons usar
                     (lambda () sala16))
               (cons entrar 
                    (lambda () sala16))
               (cons teleporte
                     (lambda () sala16)))))
(armazenar-elemento! 'portalSala27 portalSala27)

(define portalSala31
  (coisa 'portalAlfa
         #f
         (list (cons usar
                     (lambda () sala9))
               (cons entrar 
                    (lambda () sala9))
               (cons teleporte
                     (lambda () sala9)))))
(armazenar-elemento! 'portalSala31 portalSala31)

(define tesouro
  (coisa 'tesouro
         #f
         (list
          (cons pegar 
                (lambda ()
                  (begin
                    (pegar-coisa! tesouro)
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
                        (set-coisa-estado! door 'open)
                        "The door is now unlocked and open.")
                      "The door is locked.")))
          (cons close 
                (lambda ()
                  (begin
                    (set-coisa-estado! door #f)
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

;; Lugares ----------------------------------------
;; Cada lugar lida com um conjunto de verbos não-transitivos

#|
(define room
  (place
   "You're in the house."
   (list trophy)
   (list (cons out (lambda () house-front)))))
(record-element! 'room room)|#


(define sala2
  (local
    "Você se encontra em um salão com um pedestal no meio e saídas para o Leste e Oeste."
    "Azul"
    (list artefato1)
    (list
     (cons leste (lambda () sala3))
     (cons oeste (lambda () sala4)))))
(armazenar-elemento! 'sala2 sala2)

(define sala3
  (local
    "Você se encontra em um salão com um monstro e saídas para o Oeste e Norte."
    "Azul"
    (list monstroSala3)
    (list
     (cons voltar (lambda () local-anterior))
     (cons norte (lambda () (if (eq? (coisa-estado monstroSala3) #f)
                              sala8
                              "O monstro está bloqueando o caminho?")))
     (cons oeste (lambda () (if (eq? (coisa-estado monstroSala3) #f)
                              sala2
                              "O monstro está bloqueando o caminho?"))))))
(armazenar-elemento! 'sala3 sala3)

(define sala4
  (local
    "Você se encontra em um salão com um monstro e saídas para o Leste e Norte."
    "Azul"
    (list monstroSala4)
    (list
     (cons voltar (lambda () local-anterior))
     (cons norte (lambda () (if (eq? (coisa-estado monstroSala4) #f)
                              sala5
                              "O monstro está bloqueando o caminho?")))
     (cons leste (lambda () (if (eq? (coisa-estado monstroSala4) #f)
                              sala2
                              "O monstro está bloqueando o caminho?"))))))
(armazenar-elemento! 'sala4 sala4)

(define sala5
  (local
    "Você se encontra em um salão vazio com saídas para o Norte, Leste e Sul."
    "Azul"
    '()
    (list
     (cons norte (lambda () sala6))
     (cons leste (lambda () sala26))
     (cons sul   (lambda () sala4)))))
(armazenar-elemento! 'sala5 sala5)

(define sala6
  (local
    "Você se encontra em um salão com um pedestal, o qual detem um artefato, alem de visualizar saídas para: leste e sul."
    "Azul"
    (list artefato1)
    (list
     (cons leste (lambda () sala7))
     (cons sul   (lambda () sala5)))))
(armazenar-elemento! 'sala6 sala6)

(define sala7
  (local
    "Você se encontra em um salão vazio com saídas para o Oeste e Sul."
    "Azul"
    '()
    (list
     (cons oeste (lambda () sala6))
     (cons sul   (lambda () sala10)))))
(armazenar-elemento! 'sala7 sala7)

(define sala8
  (local
    "Você se encontra em um salão vazio com saídas para o Oeste e Sul."
    "Azul"
    '()
    (list
     (cons oeste (lambda () sala9))
     (cons sul   (lambda () sala3)))))
(armazenar-elemento! 'sala8 sala8)

(define sala9
  (local
    "Você se encontra em um salão com um brilho no chão (teleporte) e uma saída para o Leste."
    "Azul"
    (list portalSala9)
    (list
      (cons leste (lambda () sala8)))))
(armazenar-elemento! 'sala9 sala9)

(define sala10
  (local
    "Você se encontra em um salão com um monstro e saídas para o Leste e Norte."
    "Vermelho"
    (list monstroSala10)
    (list
     (cons voltar (lambda () local-anterior))
     (cons norte (lambda () (if (eq? (coisa-estado monstroSala10) #f)
                              sala7
                              "O monstro está bloqueando o caminho?")))
     (cons leste (lambda () (if (eq? (coisa-estado monstroSala10) #f)
                              sala11
                              "O monstro está bloqueando o caminho?"))))))
(armazenar-elemento! 'sala10 sala10)

(define sala11
  (local
    "Você se encontra em um salão vazio com saídas para o Oeste e Sul."
    "Vermelho"
    '()
    (list
      (cons oeste (lambda () sala10))
      (cons sul   (lambda () sala12)))))
(armazenar-elemento! 'sala11 sala11)

(define sala12
  (local
    "Você se encontra em um salão com um pergaminho na parede, o qual pode ser lido, e saídas para o Oeste e Norte."
    "Vermelho"
    (list dica)
    (list
      (cons norte (lambda () sala11))
      (cons oeste (lambda () sala13))
)))
(armazenar-elemento! 'sala12 sala12)

(define sala13
  (local 
    "Você se encontra em um salão vazio com saídas para o Leste e Norte."
    "Vermelho"
    '()
    (list
      (cons leste (lambda () sala12))
      (cons norte (lambda () sala14)))))
(armazenar-elemento! 'sala13 sala13)

(define sala14
  (local 
    "Você se encontra em um salão vazio com saídas para o Leste e Sul."
    "Vermelho"
    '()
    (list
      (cons leste (lambda () sala15))
      (cons sul (lambda () sala13)))))
(armazenar-elemento! 'sala14 sala14)

(define sala15
  (local 
    "Você se encontra em um salão vazio com saídas para o Oeste e Sul."
    "Laranja"
    '()
    (list
      (cons oeste (lambda () sala14))
      (cons sul (lambda () sala16)))))
(armazenar-elemento! 'sala15 sala15)

(define sala16
  (local
    "Você se encontra em um salão com um brilho no chão (teleporte) e uma saída para o Leste e o Norte."
    "Laranja"
    (list portalSala16)
    (list
      (cons leste (lambda () sala17))
      (cons norte (lambda () sala15))
)))
(armazenar-elemento! 'sala16 sala16)

(define sala17
  (local 
    "Você se encontra em um salão com um pedestal, o qual detem um artefato, alem de visualizar saídas para: oeste e norte."
    "Laranja"
    (list artefato1)
    (list
      (cons norte (lambda () sala18))
      (cons oeste (lambda () sala16)))))
(armazenar-elemento! sala17 sala17)

(define sala18
  (local
    "Você se encontra em um salão com um pergaminho na parede, o qual pode ser lido, e saídas para o Oeste e Sul."
    "Laranja"
    (list dica)
    (list
      (cons sul (lambda () sala17))
      (cons oeste (lambda () sala19))
)))
(armazenar-elemento! 'sala18 sala18)

(define sala19
  (local 
    "Você se encontra em um salão vazio com saídas para o Leste e Sul."
    "Laranja"
    '()
    (list
      (cons sul (lambda () sala20))
      (cons leste (lambda () sala18)))))
(armazenar-elemento! 'sala19 sala19)

(define sala20
  (local 
    "Você se encontra em um salão vazio, com saídas para o: Norte e Leste."
    "Amarelo"
    '()
    (list
      (cons leste (lambda () sala22))
      (cons norte (lambda () sala19)))))
(armazenar-elemento! 'sala20 sala20)

(define sala21
  (local 
    "Sala do tesouro. Você venceu o jogo."
    "Verde"
    (list tesouro)
    (list 
     (cons sair (lambda () sala2)))
    )
  )
(armazenar-elemento! 'sala21 sala21)

(define sala22
  (local
    "Você se encontra em um salão com um monstro e saídas para o Oeste e Sul."
    "Amarelo"
    (list monstroSala22)
    (list
     (cons voltar (lambda () local-anterior))
     (cons oeste (lambda () (if (eq? (coisa-estado monstroSala22) #f)
                              sala20
                              "O monstro está bloqueando o caminho?")))
     (cons sul (lambda () (if (eq? (coisa-estado monstroSala22) #f)
                              sala23
                              "O monstro está bloqueando o caminho?"))))))
(armazenar-elemento! 'sala22 sala22)

(define sala23
  (local 
    "Você se encontra em um salão vazio com saídas para o Norte e Oeste"
    "Amarelo"
    '()
    (list
     (cons norte (lambda () sala22))
     (cons oeste (lambda () sala24)))))
(armazenar-elemento! 'sala23 sala23)

(define sala24
  (local 
    "Você se encontra em um salão com um pedestal, o qual detem um artefato, alem de visualizar saídas para: Norte e Leste."
    "Amarelo"
    (list artefato1)
    (list
      (cons norte (lambda () sala25))
      (cons leste (lambda () sala23)))))
(armazenar-elemento! sala24 sala24)

(define sala25
  (local
    "Você se encontra em um salão com um monstro e saídas para o Leste e Sul."
    "Amarelo"
    (list monstroSala25)
    (list
     (cons voltar (lambda () local-anterior))
     (cons leste (lambda () (if (eq? (coisa-estado monstroSala25) #f)
                              sala21
                              "O monstro está bloqueando o caminho?")))
     (cons sul (lambda () (if (eq? (coisa-estado monstroSala25) #f)
                              sala24
                              "O monstro está bloqueando o caminho?"))))))
(armazenar-elemento! 'sala25 sala25)

(define sala26
  (local 
    "Você se encontra em um salão vazio, com saídas para o: Oeste, Norte e Sul."
    "Vermelho"
    '()
    (list
      (cons oeste (lambda () sala5))
      (cons norte (lambda () sala28))
      (cons sul (lambda () sala27)))))
(armazenar-elemento! 'sala26 sala26)

(define sala27
  (local 
    "Você se encontra em um salão com um brilho no chão (teleporte) e saídas para: Norte."
    "Vermelho"
    (list portalSala27)
    (list 
      (cons norte (lambda () sala26))
      (cons teleporte (lambda () sala16)))))
(armazenar-elemento! 'sala27 sala27)

(define sala28
  (local 
    "Você se encontra em um salão vazio, com saídas para o: Norte Leste e Sul."
    "Vermelho"
    '()
    (list
      (cons leste (lambda () sala30))
      (cons norte (lambda () sala29))
      (cons sul (lambda () sala26)))))
(armazenar-elemento! 'sala28 sala28)

(define sala29
  (local
    "Você se encontra em um salão com um pergaminho na parede, o qual pode ser lido, e saídas para o Sul."
    "Vermelho"
    (list dica)
    (list
      (cons sul (lambda () sala28)))))
(armazenar-elemento! 'sala29 sala29)

(define sala30
  (local 
    "Você se encontra em um salão vazio, com saídas para o: Oeste, Sul e Leste."
    "Laranja"
    '()
    (list
      (cons leste (lambda () sala31))
      (cons oeste (lambda () sala28))
      (cons sul (lambda () sala32)))))
(armazenar-elemento! 'sala30 sala30)

(define sala31
  (local
    "Você se encontra em um salão com um brilho no chão (teleporte) e saída para Oeste."
    "Laranja"
    (list portalSala31)
    (list
      (cons oeste (lambda () sala30)))))
(armazenar-elemento! 'sala31 sala31)

(define sala32
  (local 
    "Você se encontra em um salão vazio, com saídas para o: Norte, Leste e Sul."
    "Laranja"
    '()
    (list
      (cons leste (lambda () sala34))
      (cons norte (lambda () sala30))
      (cons sul (lambda () sala33)))))
(armazenar-elemento! 'sala32 sala32)

(define sala33
  (local
    "Você se encontra em um salão com escrituras no chão e saída para o Norte."
    "Laranja"
    (list dica)
    (list
      (cons norte (lambda () sala32))
    )
  )
)
(armazenar-elemento! 'sala33 sala33)

(define sala34
  (local
    "Você se encontra em um salão com um monstro e saídas para o: Norte Oeste e Leste."
    (list monstroSala34)
    "Amarelo"
    (list
     (cons voltar (lambda () local-anterior))
     (cons norte (lambda () (if (eq? (coisa-estado monstroSala34) #f)
                              sala35
                              "O monstro está bloqueando o caminho?")))
     (cons leste (lambda () (if (eq? (coisa-estado monstroSala34) #f)
                              sala21
                              "O monstro está bloqueando o caminho?")))
     (cons oeste (lambda () (if (eq? (coisa-estado monstroSala34) #f)
                              sala32
                              "O monstro está bloqueando o caminho?"))))))
(armazenar-elemento! 'sala34 sala34)

(define sala35
  (local
    "Você se encontra em um salão com um pergaminho e saída para o Sul."
    "Amarelo"
    (list dica)
    (list
      (cons sul (lambda () sala34))
    )
  )
)
(armazenar-elemento! 'sala35 sala35)


;; ============================================================
;; Estado do Jogo

;; Coisas carregadas pelo jogador:
(define pertences null) ; lista de coisas

;; Localização Atual:
(define local-atual sala2) ; local

;; Localização Anterior:
(define local-anterior sala2) ; local

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
  (printf "a cor da sala é: ~a\n" (local-cor local-atual)) ; imprime a cor da sala
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
                    '(sair)             ; se sim, sai
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
                  (set! local-anterior local-atual) ;; !!Labirinto!! seta o local atual como local anterior
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