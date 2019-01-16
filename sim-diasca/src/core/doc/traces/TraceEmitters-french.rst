.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex




.. _�metteur de trace:
.. _�metteurs de trace:

.. _�metteur de traces:
.. _�metteurs de traces:

.. _Emetteur:
.. _�metteur:
.. _�metteurs:



Emetteurs de traces (``TraceEmitter``)
======================================


R�le
----

Tout �metteur de traces a la facult� d'envoyer des messages qui respectent le `format de traces`_ Sim-Diasca � destination d'un `agr�gateur`_ de traces.



Impl�mentation
--------------

Un �metteur g�n�rique est d�fini via la classe ``TraceEmitter``, impl�ment�e dans ``class_TraceEmitter.hrl`` et ``class_TraceEmitter.erl``.

Pour que les instances d'une classes puissent �mettre des traces, il suffit d'utiliser l'h�ritage (multiple) permis par WOOPER :

  - inclure le fichier d'ent�te ad�quat : ``-include("class_TraceEmitter.hrl").``

  - d�clarer cette classe comme l'une des classes-m�res de la classe consid�r�e : par exemple, ``-define(wooper_superclasses,[class_TraceEmitter]).``


D�s lors diff�rents types de traces peuvent �tre �mis par les instances de cette classe, au moyen des macros suivantes, correspondant au niveau de priorit� d�fini dans le format universel : ``fatal, error, warning, info, trace, debug``.

Par exemple, l'envoi d'un message d'information se ferait ainsi : ``?info([ "Hello world!" ])``.

L'avantage de ce syst�me est que les traces sont automatiquement centralis�es (m�me dans un contexte distribu�) et que leur �mission est d�brayable [#]_.

.. [#] La d�sactivation s'obtient en commentant dans ``class_TraceEmitter.hrl`` la ligne ``-define(TracingActivated,).`` : tous les modules pr�compil�s avec cette version de la d�claration deviennent muets en terme de trace, et ceci sans qu'aucune p�nalit� en terme de performance ne subsiste.


Les macros �voqu�es (``fatal, error, warning, info, trace, debug``) utilisent implicitement une variable d'�tat nomm�e ``State``, dont les attributs contiennent diff�rentes informations n�cessaires � la cr�ation de la trace.

Cette variable est g�n�ralement partout disponible et utilisable (des m�thodes jusqu'au destructeur), sauf dans le cas du *constructeur* : d'une part il est n�cessaire que les classes-m�res aient cr�� les bases de l'objet courant pour qu'il soit utilisable (i.e. avant d'envoyer un message il faut que le constructeur de ``TraceEmitter`` ait initialis� l'�tat de l'objet de mani�re appropri�e), d'autre part il est pr�f�rable que la cat�gorisation de l'objet en terme de classe ait �t� mise � jour.

En effet, juste apr�s la construction de l'�tat par les classes-m�res, la cat�gorisation refl�te leur classe et non la classe-fille qui est en cours d'instanciation. Il faut donc que cette classe-fille mette � jour la cat�gorisation pour que les messages de traces qu'elle enverra par la suite la respecte.

Exemple de constructeur d'une classe ``class_MyClass`` directement d�riv�e de ``class_TraceEmitter`` ::

  construct(State,MyName) ->

	  % Impossible d'envoyer un message de trace pour l'instant :
	  % la classe-m�re correspondante n'a pas �t� construite.
	  TraceState = class_TraceEmitter:construct( State, MyName ),

	  % L� on peut envoyer des messages de trace, en pr�cisant
	  % un �tat qui le permet (TraceState) plut�t que l'�tat
	  % initial State.
	  % Du coup � la place de "?trace([  "Creating a new actor." ]),"
	  % on utilise :
	  ?send_trace([ TraceState, "Creating a new actor." ]),

	  % Seul probl�me : la cat�gorisation de l'�metteur est rest�e
	  % � celle de la de la classe-m�re, TraceEmitter.
	  % Il est pr�f�rable de la mettre � jour d�s que possible :
	  CategorizedState = ?setAttribute( TraceState,
	  	trace_categorization, "MyClass" ),
	  ?send_trace([ CategorizedState,
	  	"This trace is correctly categorized." ]),

	  % Fin du constructeur:
	  CategorizedState.


Pour un exemple d'utilisation complet, consulter ``class_TestTraceEmitter.erl``, pour la classe �mittrice de traces, et ``class_TraceEmitter_test.erl`` pour son test effectif.


Voir aussi :

 - `Agr�gateur`_ de traces, destinataire des traces envoy�es
 - Synth�se sur les `traces`_ Sim-Diasca
