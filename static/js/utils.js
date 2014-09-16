var edits = {};

function makeEditable(cl, dataGetter, url) {
    edits[cl] = '';
    $(cl).editable(function(value, settings) {
        var thisTd = this;
        var thisTr = $(this).parent();

        var prevValue = edits[cl];

        $.post( url
              , dataGetter(value, prevValue, thisTr)
        ).done(function(data) {
            fadeInOut("#updatesuccess");
        }).fail(function(data) {
            if(data == "exists")
            {
                fadeInOut("#updateexists");
                $(thisTd).text(prevValue);
            }
            else {
                fadeInOut("#updateerror");
                $(thisTd).text(prevValue);
            }
        }, { indicator: '<img src="/static/loading.gif" />' }
        );

        edits[cl] = '';
        return value;
    });

    $(cl).focusin(function() {
        edits[cl] = $(this).find('input').val();
    });
}


function fadeInOut(ident) {
    $(ident).fadeIn(1000, function() { 
        $(ident).fadeOut(1000); 
    });    
}

function showError(data)
{
    fadeInOut("#updateerror");
    console.log('Error: ' + data);
}

function removeRow(r)
{
    fadeInOut('#updatesuccess');
    $(r).remove();
}
