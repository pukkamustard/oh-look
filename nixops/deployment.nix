let

region = "eu-central-1";

  ec2 =
    { resources, ... }:
    { deployment.targetEnv = "ec2";
      deployment.ec2.region = region;
      deployment.ec2.instanceType = "t2.nano";
      deployment.ec2.keyPair = resources.ec2KeyPairs.my-key-pair;
      deployment.ec2.securityGroups = [ resources.ec2SecurityGroups.allow-ssh ];
    };

in
  { 
  the-island = ec2;

  # Provision an EC2 key pair.
  resources.ec2KeyPairs.my-key-pair = { 
    inherit region; 
  };

  resources.ec2SecurityGroups.allow-ssh = {
    description = "Allow SSH access";
    inherit region;
    rules = [
      {
        fromPort = 22;
        toPort = 22;
        sourceIp = "0.0.0.0/0";
      }
    ];
  };

  resources.ec2SecurityGroups.allow-server-access = {
    description = "Allow SSH access";
    inherit region;
    rules = [
      {
        fromPort = 9998;
        toPort = 9998;
        sourceIp = "0.0.0.0/0";
      }
    ];
  };




}
