<?php

class {{className}} extends Mage_Core_Model_Resource_Db_Abstract {
    protected function _construct() {
        $this->_init('{{moduleName}}/{{entityName}}', 'id');
    }
}
