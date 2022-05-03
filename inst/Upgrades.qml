
import QtQuick		2.12
import JASP.Module	1.0

Upgrades
{
	Upgrade
	{
		functionName:		"Correlation"
		fromVersion:		"0.16.2"
		toVersion:			"0.16.3"

		ChangeRename { from: "conditioningVariables";	to: "partialOutVariables" }
	}
}
