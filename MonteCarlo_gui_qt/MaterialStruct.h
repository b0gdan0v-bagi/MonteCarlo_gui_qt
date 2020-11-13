#pragma once

#include <QString>

struct MaterialStruct
{
	QString material;
	QString description;
	int iterations;
	int maxField;
	int fieldToCalc;
	int outPoints;
	int gamma;
	int temp;

	MaterialStruct(QString MATERIAL, int ITER, int MAXF, int FIELDCALC, int OUTPTS, int GAMMA, int TEMP)
		:material(MATERIAL), iterations(ITER), maxField(MAXF), fieldToCalc(FIELDCALC), outPoints(OUTPTS)
		, gamma(GAMMA), temp(TEMP) 
	{
		description = material + " n = 10^" + QString::number(iterations) + " T, K " + QString::number(temp)
			+ " max E " + QString::number(maxField) + " field to calc " +  QString::number(fieldToCalc);
	}
	MaterialStruct() {}
};
