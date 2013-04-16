
/*
 * GET home page.
 */

exports.getObjects = function(req, res){
  var id = req.params[0];
  console.log(id);
  res.set('ETag', '12345');
  res.json({ 'foo': 'bar' });
};

exports.putObjects = function(req, res){
  var id = req.params[0];
};