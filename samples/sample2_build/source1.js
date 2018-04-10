var Point = function(){this.x = undefined;this.y = undefined;};
var ArcadeConfig = function(gravity){this.gravity = gravity;};
var PhysicsConf = function(arcade){this.default = undefined;this.arcade = arcade;};
var Scene = function(preload, create){this.preload = preload;this.create = create;};
var Config = function(type, width, height, physics, scene){this.type = type;this.width = width;this.height = height;this.physics = physics;this.scene = scene;};
function preload(
){this.load.setBaseURL("./assets");
this.load.image("sky","space3.png");
this.load.image("eiffel","tour_eiffel.jpg");
this.load.image("logo","fsharp256.png");
this.load.image("red","red.png");
this.load.image("blue","blue.png");
}var Scale = function(){this.start = undefined;this.end = undefined;};
var ParticlesConfig = function(){this.speed = undefined;this.scale = undefined;this.blendMode = undefined;};
function create(
){this.add.image(400,300,"eiffel");
var scale =  new Scale(1,0);
var particlesConfig =  new ParticlesConfig(100,scale,"ADD");
var particles = this.add.particles("blue");
var emitter = particles.createEmitter(particlesConfig);
var logo = this.physics.add.image(400,100,"logo");
logo.setVelocity(100,200);
logo.setBounce(1,1);
logo.setCollideWorldBounds(true);
emitter.startFollow(logo);
}var scene =  new Scene(preload,create);
var arcade =  new ArcadeConfig( new Point(0,200));
var physics =  new PhysicsConf(arcade);
physics.default = "arcade";
var config =  new Config(Phaser.AUTO,800,600,physics,scene);
var game =  new Phaser.Game(config);
a = {12:84,"key1":54};

//# sourceURL=source1.code
//# sourceMappingURL=source1.code.map