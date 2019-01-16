.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex


.. _supervision de trace:

.. _superviseur de trace:
.. _superviseurs de trace:

.. _superviseur:
.. _superviseurs:




Superviseurs de traces (``TraceSupervisor``)
============================================


R�le
----

Un superviseur de traces permet de consulter de la mani�re la plus conviviale possible des traces de simulation, soit en cours d'ex�cution (les traces sont lues au fil de l'eau) soit de mani�re post-mortem (�tude du d�roulement d'une simulation apr�s qu'elle soit achev�e).


Outils
------

Plusieurs outils peuvent �tre employ�s pour exploiter, y compris en mode interactif, un fichier de traces (supervision de traces), du simple ``tail -f`` jusqu'� des logiciels plus avanc�s comme `LogMX <http://www.logmx.com>`_.

Pour certains outils de supervision de traces, une traduction sp�cialis�e du format universel de trace vers un format reconnu par l'outil est n�cessaire.
Pour chacun de ces formats-cibles, tous les champs du format universel sont list�s, en face desquels sont pr�cis�s leurs homologues dans le format-cible.


``tail -f``
...........

Les traces sont affich�es de mani�re brute, sans formatage, au fil de leur ajout dans le fichier de trace, issu de l'agr�gation.

LogMX
.....

Il est possible de d�finir directement le format attendu par l'outil pour les traces � partir de son interface graphique, dans le menu ``Tools -> Options -> Parsers`` en cliquant sur la croix verte et en choisissant une syntaxe Log4j pour d�crire le format d'entr�e. Cette syntaxe est d�crite `ici <http://logging.apache.org/log4j/docs/api/org/apache/log4j/PatternLayout.html>`_ et `l� <http://logging.apache.org/log4j/1.2/apidocs/org/apache/log4j/PatternLayout.html>`_, mais elle ne semble que partiellement reconnue par l'outil aujourd'hui.


Syntaxe Log4J
_____________

Les *tags* reconnus peuvent �tre d�finis � l'aide de la syntaxe `Log4J <http://logging.apache.org/log4j/1.2/apidocs/org/apache/log4j/PatternLayout.html>`_:

 - identifiant technique : (tag correspondant : ``name of the thread that generated the logging event``, ``t``)

 - nom :  (tag correspondant : ``file name where the logging request was issued``, ``F``)

 - cat�gorisation de l'�metteur : (tag correspondant : ``fully qualified class name of the caller``, ``C``)

 - horodatage :

  - temps de la simulation (tag correspondant : ``date of the logging event``, ``r``)

  - temps utilisateur (tag correspondant : ``date of the logging event``, ``d``)

 - cat�gorisation du message : (tag correspondant : ``category of the logging event``, ``c``)

 - priorit� : (tag correspondant : ``priority of the logging event``, ``p``)

 - corps du message :  (tag correspondant : ``application supplied message``, ``m``)


La correspondance, d�crite sous la forme d'expression r�guli�re Log4j, est : ``%t|%F|%C|%r|%d|%c|%p|%m``.


Malheureusement non seulement cette expression r�guli�re n'est pas reconnue, mais en plus il semble impossible aux parseurs LogMX de g�rer d'autres informations que :

 - un horodatage (``date``)

 - un niveau (``level``)

 - une "t�che" (``thread``)

 - un �metteur (``emitter``)

 - un message (``message``)


Parseur Java sp�cifique
_______________________

Suite aux limites du support par LogMX de Log4J et du nombre de champs support�s, les informations relatives � une trace sont extraites au moyen d'un parseur sp�cifique, �crit en Java, cr�� pour reconna�tre le format de trace pr�sent�.

La correspondance de repli (champs LogMX vers trace) est:

 - ``date`` : horodatage de la simulation

 - ``level`` : priorit� du message

 - ``thread`` : identifiant technique

 - ``emitter`` : cat�gorisation du type de l'�metteur (ex : ``Actor.Equipment``) augment�e du nom de l'�metteur (ex : ``MonEquipement``). Cela donne : ``Actor.Equipment.MonEquipement``

 - ``message`` : texte du message de la trace


Cela implique que les informations suivantes, relatives � une trace, ne seront pas reconnues sp�cifiquement par LogMX :

 - cat�gorisation du type du trace

 - horodatage en temps r�el de la trace

 - localisation de l'�metteur


Ces informations sont n�anmoins replac�es dans le corps du message, en d�but de texte, chacune entour�e de crochets.

Les sources du parseur Sim-Diasca correspondant sont dans ``CeylanTraceParser.java``.

Il est n�cessaire de configurer LogMX pour que les niveaux de priorit� autres que 1 et 2 soient reconnus. Pour cela il suffit de param�trer les correspondances, dans ``Tools->Levels Indirections`` :

:raw-html:`<img src="logmx-levels.png"></img>`
:raw-latex:`\includegraphics[scale=1]{logmx-levels.png}`

L'appellation des niveaux native � LogMX est identique au format universel, sauf que les informations typ�es ``Trace`` utilisent le niveau LogMX ``FINE``. Cette correspondance s'obtient via le menu ``Tools->Levels``.

Au final la console de supervision se pr�sente ainsi:


:raw-html:`<img src="logmx-interface.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{logmx-interface.png}`

L'exemple de traces utilis� est disponible sous ``TraceSample.txt``.

Pour �viter de reconstruire le parseur Sim-Diasca correspondant (n�cessite un JDK r�cent et ant), les ``bytecodes`` pr�compil�s (avec le ``JDK 1.6.0_05``) sont disponibles en t�l�chargement: `CeylanTraceParser.class``.


Les fichiers de configuration de l'outil sont � placer sous le r�pertoire ``LogMX_vx.y.z/config``. Ce sont les fichiers suivants :

 - ``logmx.properties``

 - ``managers.properties``

 - ``parsers.properties``




L'outil LogMX a �t� int�gr� au simulateur Sim-Diasca en tant que superviseur de traces directement pilotable (lancement, arr�t, attente de la fin de consultation des traces, transmission du fichier de traces g�n�r�s par l'agr�gateur de traces, etc.) via Erlang et, par exemple, depuis les tests.



Impl�mentation
--------------

L'impl�mentation correspondante du superviseur de traces fond� sur LogMX est dans ``class_TraceSupervisor.hrl`` et ``class_TraceSupervisor.erl``.


Voir aussi :

 - `Agr�gateur`_ de traces, destinataire des traces envoy�es

 - Synth�se sur les `traces`_ Sim-Diasca
