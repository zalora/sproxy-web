postUrl = '/rename-user';

makeEditable('.email-edit', renameUser, postUrl);

function renameUser(newEmail, prevEmail, tr) {
  var obj = { old_email: prevEmail, new_email: newEmail };
  return obj;
}
