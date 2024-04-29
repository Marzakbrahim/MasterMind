       IDENTIFICATION DIVISION.
       PROGRAM-ID. MasterMind.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

      * Tableau pour stocker la première saisie :
       01 Tab-Personne1.
           05 Pers1                 PIC X OCCURS 3 INDEXED BY IND-TAB1.

      * Tableau pour stocker la deuxième saisie :
       01 Tab-Personne2.
           05 Pers2                 PIC X OCCURS 3 INDEXED BY IND-TAB2.

      * Variable pour compter combien de chiffre dans la proposition est bien placé.
       01 bien-place                        PIC 9(2).

      * Variable pour compter combien de chiffre dans la proposition est trouvé.
       01 chiffres-trouves                  PIC 9(2).

      * Un booléan pour arrêter le jeu dans le cas où la 2eme personne a trouvé le bon nombre.
       01 Gagne                             PIC X(3).
           88 Gagne-oui                     VALUE 'OUI'.
           88 Gagne-non                     VALUE 'NON'.

      * Indices où il y a des bien placés :
       01 Indices-BP.
           05 IBP                   PIC 9 OCCURS 3.
      * Indice
       01 I                         PIC 99.

      * Indices où il y a des éléments trouvés :
       01 elem-trouves.
           05 ELTR                 PIC 9 OCCURS 3.

      * Nombre de tentatives pour forcer l'arrêt aprês 10 tentatives (perdu !):
       01 Nbr-tent  PIC 9(2).


      * utilisable dans le co,trôle pour savoir quelle saisie je traite.
       01 Quel-etape                  PIC 9.
           88 ETP1                    VALUE 1.
           88 ETP2                    VALUE 2.
           88 PARDEF                    VALUE 0.

      * Le cas où le deuxième utilisateur veut s'arrêter.
       01 Arret PIC X(3).
           88 Arret-Oui    VALUE 'OUI'.
           88 Arret-Non    VALUE 'NON'.

      * Si on trouve un chiffre on arrête de chercher :
       01 Trouve                      PIC X.
           88 Trouve-oui              VALUE 'Y'.
           88 Trouve-non              VALUE 'N'.
       01 INDICE-REP                  PIC 9.


       PROCEDURE DIVISION.

      *********************
       PROGRAMME-PRINCIPAL.
      *********************
      * L'intéligence général du programme.
           PERFORM INITIALISATION-DEB THRU INITIALISATION-FIN
           PERFORM TRAITEMENTS-DEB THRU TRAITEMENTS-FIN
           PERFORM FIN
           .


      ********************
       INITIALISATION-DEB.
      ********************
      * Initialisation des variables (si pas de valeur assignée lors
      * de la déclaration), ouverture des fichier, premiere lecture,
      * affichage d'une fenêtre de début de programme.
           INITIALISE Tab-Personne1
                      Tab-Personne2
                      chiffres-trouves
                      bien-place
                      Nbr-tent
                      Indices-BP
                      I
                      INDICE-REP
                      elem-trouves

           SET PARDEF   TO TRUE
           SET Arret-Non TO TRUE
           SET Trouve-non TO TRUE
           SET Gagne-non TO TRUE

           DISPLAY '**********************'
           DISPLAY '*** INITIALISATION ***'
           DISPLAY '**********************'
           .
      **************************
       INITIALISATION-FIN. EXIT.
      **************************

      ***************
       CONTROLES-DEB.
      ***************
           EVALUATE TRUE
             WHEN  Tab-Personne1 NOT NUMERIC AND ETP1
               DISPLAY "Attention, votre saisie n'est pas valide !"
      *         PERFORM TRAITEMENTS-DEB THRU TRAITEMENTS-FIN
               PERFORM FIN
             WHEN Tab-Personne2 NOT NUMERIC  AND ETP2
                                         AND Tab-Personne2 NOT = "FIN"
               DISPLAY "Attention, votre saisie n'est pas valide !"
      *         PERFORM TRAITEMENTS-DEB THRU TRAITEMENTS-FIN
                PERFORM FIN
           END-EVALUATE
           .
      *********************
       CONTROLES-FIN. EXIT.
      *********************



      *****************
       TRAITEMENTS-DEB.
      *****************
           PERFORM RecevoirNum1 THRU FIN-RecevoirNum1
           PERFORM CONTROLES-DEB THRU CONTROLES-FIN
           PERFORM UNTIL Gagne-oui OR Nbr-tent = 10 OR Arret-Oui
             PERFORM RecevoirNum2 THRU FIN-RecevoirNum2
             IF Arret-Oui
               PERFORM FIN
             END-IF
             PERFORM CONTROLES-DEB THRU CONTROLES-FIN
             PERFORM Affichage THRU FIN-Affichage
             ADD 1 TO Nbr-tent
           END-PERFORM
           IF Nbr-tent =10
             DISPLAY "***** Vous avez PERDU ! HAHAHAHAHAHA ****"
           END-IF
           .
      ************************
       TRAITEMENTS-FIN. EXIT.
      ************************



      *------------------------------------------------------------------------
      **************
       RecevoirNum1.
      **************
           DISPLAY 'Premier Personne : Veuillez entrer un nombre de 3'
           ' chiffres sans que ton ami le voit !!!'
      *     ACCEPT Personne1
           ACCEPT Tab-Personne1
           SET ETP1 TO TRUE
           .
      ******************
       FIN-RecevoirNum1. EXIT.
      ******************

      **************
       RecevoirNum2.
      **************
           DISPLAY '2eme Personne : Veuillez entrer votre proposition '
           DISPLAY "de 3 chiffres ou bien ecrit 'FIN' pour arreter"
                   " le jeu. "
      *     ACCEPT Personne2
           ACCEPT Tab-Personne2
           SET ETP2 TO TRUE
           IF Tab-Personne2 = "FIN"
             SET Arret-Oui TO TRUE
             DISPLAY "Vous avez abandonne !"
           END-IF
           .
      ******************
       FIN-RecevoirNum2. EXIT.
      ******************
      *------------------------------------------------------------------------


      ***********
       Affichage.
      ***********
      *    Traitement des chiffres bien placés :
           MOVE 1 TO I
           SET IND-TAB1 TO 1
           PERFORM VARYING IND-TAB1 FROM 1 BY 1
                                         UNTIL IND-TAB1 > 3
             IF Pers1(IND-TAB1) = Pers2(IND-TAB1)
               ADD 1 TO bien-place
               MOVE IND-TAB1 TO IBP(I)
               ADD 1 TO I
             END-IF
           END-PERFORM

      *    Traitement des chiffres trouvés non bien placés :
           SET IND-TAB1 TO 1
           PERFORM Recherche THRU FIN-Recherche
           VARYING IND-TAB1 FROM 1 BY 1
           UNTIL IND-TAB1 > 3

      *    Affichage :
           EVALUATE TRUE
             WHEN Gagne-oui
               DISPLAY "Vous avez trouve ! Bravo !"
             WHEN bien-place = ZERO
               DISPLAY chiffres-trouves " chiffres trouves."
             WHEN chiffres-trouves = ZERO AND NOT Gagne-oui
               DISPLAY bien-place " bien places."
             WHEN bien-place NOT =ZERO AND chiffres-trouves NOT = ZERO
               DISPLAY bien-place " chiffres bien places et "
                       chiffres-trouves " chiffres  trouves."
           END-EVALUATE
           INITIALISE bien-place
                      chiffres-trouves
                      Indices-BP
           .
      ***************
       FIN-Affichage. EXIT.
      ***************


      ******************
       Recherche.
      ******************
           MOVE 1 TO I
           SET IND-TAB2 TO 1
           PERFORM VARYING IND-TAB2 FROM 1 BY 1
                              UNTIL IND-TAB2 > 3 OR Trouve-oui
             EVALUATE TRUE
               WHEN (Pers1(IND-TAB1) = Pers2(IND-TAB2)
                     AND IND-TAB1 NOT = IBP(1)
                     AND IND-TAB1 NOT = IBP(2)
                     AND IND-TAB1 NOT = IBP(3)
                     AND INDICE-REP NOT =IND-TAB2)
                 IF IND-TAB1 NOT = IND-TAB2
                   ADD 1 TO chiffres-trouves
                   MOVE Pers2(IND-TAB2) TO ELTR(I)
                   ADD 1 TO I
                   SET Trouve-oui TO TRUE
                   MOVE IND-TAB2 TO INDICE-REP
                 END-IF
               WHEN OTHER
                 CONTINUE
             END-EVALUATE
           END-PERFORM
           SET Trouve-non TO TRUE

      *    Forcer l'arrêt :
           IF Tab-Personne1 = Tab-Personne2
             SET Gagne-oui TO TRUE
           END-IF
           .

      ******************
       FIN-Recherche.
      ******************

      ******
       FIN.
      ******
           DISPLAY 'Fin de traitement.'
           STOP RUN.
       END PROGRAM MasterMind.
