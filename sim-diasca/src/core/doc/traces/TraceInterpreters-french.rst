.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex



.. _interpr�teur de trace:
.. _interpr�teurs de trace:

.. _interpr�teur:
.. _interpr�teurs:



Interpr�teurs de traces (``TraceInterpreter``)
==============================================


R�le
----

Les interpr�teurs de traces facilitent le d�pouillement des r�sultats par l'extraction et le traitement des traces enregistr�es.

Cela peut se faire au moyen de requ�tes sur une base de traces, dont les r�ponses sont trait�es par un module de corr�lation et de visualisation de r�sultats.


Impl�mentation & Outils
-----------------------

Un premier niveau d'interpr�tation peut �tre obtenu au moyen de certains outils de `supervision de trace`_. Ainsi les vues synth�tiques de LogMX et ses corr�lations temporelles �l�mentaires fournissent des indicateurs utiles.

Pour une exploitation moins limit�e des traces, il sera vraisemblablement n�cessaire de se fonder d'une part sur un `enregistreur` de trace �volu� (ex: permettant un requ�tage type base de donn�es), d'autre part sur un module de traitement et de visualisation des traces extraites qui serait � embarquer dans l'interpr�teur de traces �voqu� ici. Ces d�veloppements restent � faire.
