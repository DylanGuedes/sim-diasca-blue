.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex


.. _format de traces:
.. _�metteurs de trace:
.. _traces:


Traces logicielles des simulateurs de SI
========================================


Objectif poursuivi
------------------

Adopter un format de traces (de logs) permet de d�coupler l'ex�cution d'une simulation de l'interpr�tation de son d�roulement (*post-processing* type analyse statistique).

Si en plus le format choisi est pens� pour �tre "universel", pour ne pas d�pendre d'un g�n�rateur particulier de ces traces ou d'un cas m�tier, alors plusieurs simulateurs peuvent l'utiliser et partager toute la cha�ne en aval de la g�n�ration des traces.

Notons que les traces permettent de suivre une ex�cution, sans pour autant constituer n�cessairement la seule forme de r�sultat de la simulation ni avoir vocation � traduire tous les �v�nements survenus : l'historisation d'�v�n�ments de simulation est un sujet plus vaste et qui ne s'incarne pas n�cessairement sous forme de traces.

Le d�coupage des pr�rogatives en terme de gestion r�partie des traces est le suivant :

 - `�metteurs`_ de traces : tout objet simul�, mais aussi tout composant de service technique, peut �tre amen� � enregistrer des traces relatives � son fonctionnement

 - `agr�gateurs`_ de traces : s'ils peuvent �tre en nombre quelconque (par type d'objet, par machine, etc.), la solution la plus usuelle est de n'utiliser qu'un seul agr�gateur, qui centralise toutes les traces de tous les �metteurs distribu�s. Ainsi, analyse d'ensemble et recherche de corr�lation sont possibles

 - `superviseurs`_ de traces : ils permettent d'effectuer un suivi en temps r�el de la vie du syst�me, via ses traces. Plusieurs outils sont disponibles pour cela. Le suivi peut s'effectuer � distance (sur une autre machine que les calculateurs impliqu�s) et/ou de mani�re *post-mortem*, une fois la simulation finie

 - `enregistreurs`_ de traces : ils assurent le stockage persistant des traces, sous forme de simple fichier texte ou d'une base de donn�es requ�table, potentiellement temps r�el et r�partie

 - `interpr�teurs`_ de traces : ils facilitent le d�pouillement des r�sultats par l'extraction et le traitement des traces enregistr�es. Cela peut se faire au moyen de requ�tes sur une base de traces dont les r�ponses sont trait�es par un module de visualisation de r�sultats



Description des informations d�taill�es dans une trace
------------------------------------------------------


Dans le format de trace "pivot", il serait int�ressant de faire figurer les informations suivantes :

 - informations portant sur l'*�metteur* du message :

  - **identifiant technique** de l'�metteur, g�n�ralement sp�cifique � une plate-forme de simulation. Par exemple avec Erlang ce serait un num�ro de processus (PID) du type ``<0.31.0>``

  - **nom** de l'�metteur, unique dans le contexte d'une simulation donn�e, construit pour servir de niveau d'indirection par rapport � un identifiant technique de type identifiant num�rique d'un agent ou PID d'un processus dans une plate-forme donn�e. Format : suite de caract�res, alphanum�riques ou dans ``-_+=.#&@``. Par exemple : ``MyObject-17``. Dans le contexte d'une simulation donn�e, une bijection existe entre les identifiants techniques et les noms. En revanche, pour un m�me cas de simulation ex�cut� deux fois, si les noms doivent �tre stables (pour assurer la reproductibilit�), il n'en est pas n�cessairement de m�me pour les identifiants techniques

  - **cat�gorisation de l'�metteur**, selon une hi�rarchie de cat�gories, pr�cis�e ci-dessous. Par exemple : ``Actor.Equipment.MyObject.SpecificObject``, ou ``Service.Scheduler``. Si un �metteur n'a pas d�fini sa cat�gorisation, elle sera fix�e � ``NotCategorized``

  - **localisation de l'�metteur**, via un nom de noeud logique et/ou un nom de machine. Par exemple : ``sim_diasca_test@myhost.org``. Si aucun nom pertinent n'est trouv� (ex : la machine n'a pas de nom r�seau), ``localhost`` est utilis�

 - informations portant sur le *message lui-m�me* :

  - **horodatage** du message en temps de :

   - la simulation, sous la forme d'un (grand) entier positif, de ``none`` si la simulation n'est pas en cours ou de ``unknown`` si l'�metteur n'a pas pr�cis� le pas de temps (que la simulation soit en cours ou non). Par exemple, ``3168318240218``

   - l'utilisateur, sous la forme ``dd/mm/yyyy hh:MM:ss``. Par exemple, ``08/04/2008 04:41:24``

  - **cat�gorisation du message**, selon une hi�rarchie de cat�gories, pr�cis�e ci-dessous. Par exemple : ``Simulation.Topic.Event`` ou ``System.Start``

  - niveau de **priorit�** de la trace (importance, criticit�, s�v�rit�, etc.), nombre entier strictement positif. Les messages les plus prioritaires ont les niveaux de priorit� les plus faibles. Aux premiers niveaux de priorit� sont associ�s des noms :

+------------------------+------------+
| Niveau de priorit�     | Nom        |
+========================+============+
| 1                      | ``Fatal``  |
+------------------------+------------+
| 2                      | ``Error``  |
+------------------------+------------+
| 3                      | ``Warning``|
+------------------------+------------+
| 4                      | ``Info``   |
+------------------------+------------+
| 5                      | ``Trace``  |
+------------------------+------------+
| 6                      | ``Debug``  |
+------------------------+------------+

  - **corps du message** lui-m�me, sous forme d'une liste de caract�res, de longueur variable (potentiellement nulle, potentiellement comprenant des retours � la ligne)

Il serait envisageable que, selon son type, une trace comporte des attributs suppl�mentaires. Toutefois dans ce cas on se placerait plut�t alors dans un syst�me d'historisation d'�v�nements, tel qu'�voqu� en introduction.



Cat�gorisation des �metteurs
----------------------------

Un �metteur de trace est identifi� par une liste de rubriques, de la plus g�n�rale � la plus pr�cise, s�par�es par un point.

Ces rubriques sont class�es de mani�re arborescente, par exemple selon le d�but de hi�rarchie suivant :

 - Actor

  - StochasticActor

   - FailureModel

   - RepairModel

  - Equipment

 - SimulationService

  - TimeManager

  - RandomManager

  - SimulationManager
  - ScenarioManager

  - TraceService

   - TraceEmitter

   - TraceAggregator

   - TraceMonitor

   - TraceRecorder

   - TraceInterpreter

 - Probe

 - NotCategorized, si aucune cat�gorie n'a �t� stipul�e

Ainsi un objet de type ``MyType``, de mod�le ``MyDesign`` et de marque ``MyTrademark`` peut �tre class� dans la rubrique ``Actor.Equipment.MyType.MyTrademark.MyDesign``.



Cat�gorisation des messages
---------------------------

Les cat�gories d'information peuvent �tre class�es selon le d�but de hi�rarchie suivant :

 - ``System`` : traces -techniques- du simulateur, comme le franchissement d'un pas de temps

  - ``Management``

   - ``SimulationStart``

   - ``SimulationResume``

   - ``SimulationContinue``

   - ``SimulationStop``

   - ``SimulationSave``

   - ``SimulationLoad``

  - ``Time`` : pour les messages en lien avec la gestion du temps simul�, l'ordonnancement :


   - ``BeginningOfTick``
   - ``EndOfTick``

  - ``Random`` : en lien avec la gestion du hasard

  - ``Lifecycle`` : en lien avec la cr�ation, l'initialisation, la migration, la suppression d'instances

 - ``Simulation`` : traces "m�tier", portant sur les �l�ments simul�s, comme le changement d'�tat d'un objet m�tier

  - ``Discovery`` : pour les messages en lien avec la d�couverte d'�quipements

  - ``Reading`` : en lien avec la collecte de donn�es

  - ``Update`` : en lien avec la mise � jour du syst�me

  - ``Failure`` : en lien avec la d�faillance d'�quipements

  - ``Repair`` : en lien avec la r�paration d'�quipements

  - ``State`` : en lien avec la gestion de l'�tat des acteurs simul�s

  - ``Communication`` : en lien avec la communication entre acteurs

  - ``Uncategorized`` : pour les messages pour lesquels aucune cat�gorie n'a �t� mentionn�e


Ainsi un message portant sur la panne des �quipements pourrait �tre cat�goris� via ``Simulation.Repair``.


Format "universel" d'une trace
------------------------------

Ce format, d�fini par Sim-Diasca, se veut g�n�rique, et s'appliquer � tout type de simulation, d'o� l'appellation *universel*.

Chaque trace est stock�e dans une seule ligne de texte logique, de longueur arbitraire mais sans caract�re de retour � la ligne.

Le s�parateur de champ retenu est *pipe* (``|``). Il ne doit pas figurer dans le corps du message.

Le format correspondant est :

  ``identifiant technique|nom|cat�gorisation de l'�metteur|horodatage simulation|horodatage utilisateur|localisation|cat�gorisation du message|priorit�|corps du message``


Par exemple :

  ``<0.31.0>|MyObject-17|Actor.Equipment.MyObject.SpecificObject|3168318240218|08/04/2008 04:41:24|sim_diasca_test@myhost.org|Simulation.Discovery.LostConnection|2|No answer from device``


Voir aussi :

 - `�metteurs`_ de traces
 - `agr�gateurs`_ de traces
 - `superviseurs`_ de traces
 - `enregistreurs`_ de traces
 - `interpr�teurs`_ de traces


Le syst�me de traces peut �tre test� en isolation en ex�cutant ``traceManagement_test.erl``.
