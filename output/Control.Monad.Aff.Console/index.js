// Generated by psc version 0.9.1
"use strict";
var Prelude = require("../Prelude");
var Control_Monad_Eff_Console_1 = require("../Control.Monad.Eff.Console");
var Control_Monad_Eff_Console_1 = require("../Control.Monad.Eff.Console");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var warnShow = function (dictShow) {
    return function ($4) {
        return Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Console_1.warnShow(dictShow)($4));
    };
};
var warn = function ($5) {
    return Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Console_1.warn($5));
};
var logShow = function (dictShow) {
    return function ($6) {
        return Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Console_1.logShow(dictShow)($6));
    };
};
var log = function ($7) {
    return Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Console_1.log($7));
};
var infoShow = function (dictShow) {
    return function ($8) {
        return Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Console_1.infoShow(dictShow)($8));
    };
};
var info = function ($9) {
    return Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Console_1.info($9));
};
var errorShow = function (dictShow) {
    return function ($10) {
        return Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Console_1.errorShow(dictShow)($10));
    };
};
var error = function ($11) {
    return Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Console_1.error($11));
};
module.exports = {
    error: error, 
    errorShow: errorShow, 
    info: info, 
    infoShow: infoShow, 
    log: log, 
    logShow: logShow, 
    warn: warn, 
    warnShow: warnShow
};