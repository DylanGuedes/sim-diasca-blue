.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex



.. _agr�gateur de trace:
.. _agr�gateurs de trace:

.. _agr�gateur:
.. _agr�gateurs:



Agr�gateurs de traces (``TraceAggregator``)
===========================================


R�le
----

La fonction d'un agr�gateur de traces est de centraliser des traces en provenance d'un ensemble d'�metteurs de traces, potentiellement localis�s sur des calculateurs distincts.

Si les agr�gateurs peuvent �tre en nombre quelconque (par type d'objet, par machine, etc.), la solution la plus usuelle est de n'utiliser qu'un seul agr�gateur, qui centralise l'int�gralit� des traces de tous les �metteurs distribu�s. Ainsi, l'analyse d'ensemble et la recherche de corr�lation sont possibles.



Impl�mentation
--------------

Un agr�gateur g�n�rique est d�fini via la classe ``TraceAggregator``, impl�ment�e dans ``class_TraceAggregator.hrl`` et ``class_TraceAggregator.erl``.

Cet agr�gateur est naturellement r�parti (il agr�ge toutes les traces envoy�es de mani�re concurrente et distribu�es sur plusieurs machines). Il refl�te la temporalit� du syst�me dans une logique d'engagement de moyen (*best effort*) avec pour granularit�, quand les �metteurs sont des acteurs (``Actor``), un pas de temps : des messages envoy�s lors de pas de temps distincts seront n�cessairement class�s dans le bon ordre, mais � l'int�rieur d'un pas de temps aucun ordre total n'est garanti.


Voir aussi :

 - `Emetteur`_ de traces, source des traces agr�g�es

 - Synth�se sur les `traces`_ Sim-Diasca
