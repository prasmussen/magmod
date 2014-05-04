<?php

$installer = $this;
$installer->startSetup();
$connection = $installer->getConnection();

{{#entities}}
// Define {{name}} table
${{name}}Table = $connection->newTable($installer->getTable('{{moduleName}}/{{name}}'))
    ->addColumn('id', Varien_Db_Ddl_Table::TYPE_INTEGER, null, array(
        'identity' => true,
        'nullable' => false,
        'primary' => true,
    ), 'Id');


{{/entities}}

{{#entities}}
$connection->createTable(${{name}}Table);
{{/entities}}

$installer->endSetup();
