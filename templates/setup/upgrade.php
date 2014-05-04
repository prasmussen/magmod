<?php

$installer = $this;
$installer->startSetup();
$connection = $installer->getConnection();

{{#entities}}
// Update {{name}} table
${{name}}Table = $installer->getTable('{{moduleName}}/{{name}}');

$connection->addColumn(${{name}}Table, 'some_column_name', array(
    'type'     => Varien_Db_Ddl_Table::TYPE_TEXT,
    'nullable' => false,
    'default'  => '',
    'comment'  => 'Some column comment',
));


{{/entities}}
$installer->endSetup();
