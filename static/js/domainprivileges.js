postUrl = '/domain/' + domain + '/privileges';

makeEditable('.privilege-edit', privilegeUpdate, postUrl);

function privilegeUpdate(newVal, prevVal, tr) {
    var obj = { operation: 'upd'
              , new: newVal 
              , old: prevVal 
              };

    return obj;
}


$('.add-btn').click(function() {
    var currRow       = $(this).parent().parent();
    var currPrivilege = $(currRow).children("td").eq(0).find('input').val();

    if (currPrivilege.length < 1) {
        fadeInOut('#fieldempty');
    }
    else {
        sendPrivilege(currPrivilege, appendRow, showError, currRow);
    }
});

$('.delete-btn').click(function() {
    var hisTr     = $(this).parent().parent();
    var privilege = $(hisTr).children("td").eq(0).text();

    deletePrivilege(privilege, hisTr, removeRow, showError);
});

$('.rule-btn').click(function() {
    var hisTr = $(this).parent().parent();
    var privilege = $(hisTr).children("td").eq(0).text();

    window.location = '/domain/' + domain + '/privilege/' + privilege + '/rules';
});

function sendPrivilege(privilegeField, onSucc, onErr, addRow) {
    $.post( postUrl 
          , { operation: "add", privilege: privilegeField } 
    ).done(function(data) {
        if(data == "added") {
            onSucc(privilegeField, addRow);
        }
        else {
            onErr(data);
        }
    }).fail(onErr);
}

function deletePrivilege(privilegeField, row, onSucc, onErr) {
    $.post( postUrl 
          , { operation: "del", privilege: privilegeField }
    ).done(function(data) {
        if(data == "deleted")
        {
            onSucc(row);
        }
        else {
            onErr(data);
        }
    }).fail(onErr);
}

function appendRow( privilegeField, addRow)
{
    fadeInOut("#updatesuccess");

    $('#edittable tbody').append(
        '<tr><td class="edit privilege-edit">' + privilegeField + '</td>' +
        '<td><a href="#" class="delete-btn btn btn-danger btn-xs">Delete</a></td>' + 
        '<td><a href="#" class="rule-btn btn btn-link btn-xs">Rules</a></td></tr>');

    $(addRow).children("td").eq(0).find('input').val("");

    makeEditable('.privilege-edit', privilegeUpdate, postUrl);

    $('.delete-btn').click(function() {
        var hisTr     = $(this).parent().parent();
        var privilege = $(hisTr).children("td").eq(0).text();

        deletePrivilege(privilege, hisTr, removeRow, showError);
    });

    $('.rule-btn').click(function() {
        var hisTr  = $(this).parent().parent();
        var privilege = $(hisTr).children("td").eq(0).text();

        window.location = '/domain/' + domain + '/privilege/' + privilege + '/rules';
    });
}

// for the table on the right

// array1 - array2
// i.e what elements are in array1 but not in array2
function arrayDiff(array1, array2)
{
    return $(array1).not(array2).get();
}

// we refresh groups/privileges every 5 seconds
var refreshRate = 5000;

var groupsUrl = '/groups.json';

function getGroups()
{
    $.get(groupsUrl).done(function(data) {
        var newGroups = data; // $.parseJSON(data);
        var currentGroups = $('#groupSel > option').map(function() { return this.value; }).get();
        var added   = arrayDiff(newGroups, currentGroups);
        var removed = arrayDiff(currentGroups, newGroups);
        // we add the new options to the select and remove the ones that got removed
        added.forEach(addGroupOption);
        removed.forEach(removeGroupOption);
    });
}

function addGroupOption(opt)
{
    $('#groupSel').append('<option value="' + opt + '">' + opt + '</option>');
}

function removeGroupOption(groupName)
{
    $('#groupSel option[value="' + groupName + '"]').remove();
}

var privsUrl = '/domain/' + domain + '/privileges.json';

function getPrivileges()
{
    $.get(privsUrl).done(function(data) {
        var newPrivs = data; // $.parseJSON(data);
        var currentPrivs = $('#privSel > option').map(function() { return this.value; }).get();
        var added   = arrayDiff(newPrivs, currentPrivs);
        var removed = arrayDiff(currentPrivs, newPrivs);
        // we add the new options to the select and remove the ones that got removed
        added.forEach(addPrivOption);
        removed.forEach(removePrivOption);
    });
}

function addPrivOption(opt)
{
    $('#privSel').append('<option value="' + opt + '">' + opt + '</option>');
}

function removePrivOption(privName)
{
    $('#privSel option[value="' + privName + '"]').remove();
}

/* This seem to be a problem in some browsers
$('#privSel').focusin(function() {
    getPrivileges();
}); 

$('#groupSel').focusin(function() {
    getGroups();
});
*/

// so we're just updating every few seconds instead
setInterval(getGroups, refreshRate);
setInterval(getPrivileges, refreshRate);

var gpUrl = '/domain/' + domain + '/group_privileges'

$('.grant-btn').click(function() {
    var currRow = $(this).parent().parent();
    var currGr  = $(currRow).children("td").eq(0).find('select').val();
    var currPr  = $(currRow).children("td").eq(1).find('select').val();

    sendGroupPriv(currGr, currPr, appendGPRow, gpErr, currRow);
});

function sendGroupPriv(groupField, privField, onSucc, onErr, btnRow) {
    $.post( gpUrl 
          , { operation: "add", group: groupField, privilege: privField } 
    ).done(function(data) {
        if(data == "added") {
            onSucc(groupField, privField, btnRow);
        }
        else {
            onErr(data);
        }
    }).fail(onErr);
}

function appendGPRow(group, priv, btnRow)
{
    fadeInOut("#gpsuccess");

    $('#edittable2 tbody').append(
        '<tr><td>' + group + '</td>' + 
            '<td>' + priv  + '</td>' + 
            '<td><a href="#" class="delete-gp-btn btn btn-danger btn-xs">Delete</a></td></tr>');

    $('.delete-gp-btn').click(function() {
        var hisTr = $(this).parent().parent();
        var group = $(hisTr).children("td").eq(0).text();
        var priv  = $(hisTr).children("td").eq(1).text();

        deleteGP(group, priv, hisTr, removeGPRow, gpErr);
    });

}

function gpErr(err)
{
    fadeInOut("#gperror");
    console.log(err);
}

$('.delete-gp-btn').click(function() {
    var hisTr = $(this).parent().parent();
    var group = $(hisTr).children("td").eq(0).text();
    var priv  = $(hisTr).children("td").eq(1).text();

    deleteGP(group, priv, hisTr, removeGPRow, gpErr);
});

function removeGPRow(r)
{
    fadeInOut("#gpsuccess");
    $(r).remove();
}

function deleteGP(groupF, privF, row, onSucc, onErr)
{
    $.post( gpUrl 
          , { operation: "del", group: groupF, privilege: privF }
    ).done(function(data) {
        if(data == "deleted")
        {
            onSucc(row);
        }
        else {
            onErr(data);
        }
    }).fail(onErr);
}