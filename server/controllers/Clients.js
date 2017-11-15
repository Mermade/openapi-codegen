console.log('loaded');

function clientOptions(req,res) {
    res.end('OK');
}

function clientGet(req,res) {
    console.log('called');
    res.end('Several');
}

module.exports = {
    clientGet: clientGet,
    clientOptions: clientOptions
};
