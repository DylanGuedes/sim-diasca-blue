.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex


.. _enregistreur de trace:
.. _enregistreurs de trace:

.. _enregistreur:
.. _enregistreurs:



Enregistreurs de traces (``TraceRecorder``)
===========================================


R�le
----

Les enregistreurs de traces assurent le stockage persistant des traces, sous forme de simple fichier texte ou d'une base de donn�es requ�table, potentiellement temps r�el et r�partie.


Impl�mentation
--------------

A l'heure actuelle les traces sont directement enregistr�es dans un simple fichier par l'`agr�gateur`_ de traces utilis�.

A terme il s'agira de les enregistrer dans une base de donn�es (ex: ``Mnesia``), au moyen d'un agr�gateur de traces �volu�.
