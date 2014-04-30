<?php

class {{className}}_Collection extends Mage_Core_Model_Resource_Db_Collection_Abstract {
    public function _construct() {
        $this->_init('{{moduleName}}/{{entityName}}');
    }
}
