
/*
 * GET home page.
 */

exports.getSubs = function(req, res){
  var id = req.params[0];
  console.log(id);
  res.set('ETag', '12345');
  res.json({ 'foo': 'bar' });
};

exports.putSubs = function(req, res){
  var id = req.params[0];
};