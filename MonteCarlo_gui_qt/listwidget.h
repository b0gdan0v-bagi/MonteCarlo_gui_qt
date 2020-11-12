#pragma once

#include <QWidget>
#include <QPushButton>
#include <QListWidget>
#include <fstream>
#include <sstream>
#include <QTextStream>
#include <QFile>
#include <QMainWindow>

#include <QLineEdit>
#include <QLabel>

QT_BEGIN_NAMESPACE
class QAction;
class QMenu;
class QPlainTextEdit;
class QSessionManager;

class QCheckBox;
class QComboBox;
class QGroupBox;
class QLabel;
class QSpinBox;
class QStackedWidget;
QT_END_NAMESPACE

class ListWidget : public QWidget {

    Q_OBJECT

public:
    ListWidget(QWidget* parent = 0);

private slots:

    void testItem();
    void saveItem();
    void addItem();
    void renameItem();
    void removeItem();
    void clearItems();

    void setRoomT();
    void setHeliumT();
    void setNitrogenT();

private:
    QListWidget* lw;

    QLabel* materialLabel;
    QComboBox* materialCB;

    QLabel* iterationsLabel;
    QSpinBox* iterationsSB;

    QLabel* maxFieldLabel;
    QLineEdit* maxFieldLE;
    QLabel* numberFieldsToCalcLabel;
    QSpinBox* numberFieldsToCalcSB;

    QLabel* outputPointsLabel;
    QLineEdit* outputPountsLE;

    QLabel* gammaLabel;
    QSpinBox* gammaSB;

    /*QSlider* ownSlider;
    QSpinBox* myOwnSpinBox;
    QLabel* valueLabelForMyOwnSpinBox;
    QComboBox* myFirstComboBox;
    QLineEdit* myFirstLineEdit;*/

    QLabel* temperatureLabel;
    QLineEdit* temperatureLE;
    QPushButton* nitrogenT;
    QPushButton* roomT;
    QPushButton* heliumT;


    QPushButton* testButton;
    QPushButton* save;
    QPushButton* add;
    QPushButton* rename;
    QPushButton* remove;
    QPushButton* removeAll;

    void view_debug(const char* pszFileName);


};
