#!/usr/bin/env ruby

require 'shellwords'
require 'FileUtils'

LINK_MAP = {
    "bashrc" => ".bashrc",
    "coffeelint-config.json" => ".coffeelint.json",
    "emacs" => ".emacs",
    "git-completion.sh" => ".git-completion.sh",
    "emacs.d" => ".emacs.d",
    "inputrc" => ".inputrc",
    "ssh-config" => ".ssh-config"
}

DIRECTORIES = [
  ".emacs_autosaves",
  ".emacs_backups",
  ".ssh/sockets"
]

BREW_PACKAGES = [
  'git',
  'wget',
  'mobile-shell',
]

def yes_or_no_prompt(prompt, opts={})
  yn = opts[:default_y] ? "Y/n" : "y/N"
  print "#{prompt} (#{yn}) "
  reply = STDIN.gets.chomp
  if opts[:default_y]
    !(reply =~ /^no?$/i)
  else
    reply =~ /^y(es)?$/i
  end
end

def backup_file(path)
  target = "./backups/#{File.basename(path)}.#{Time.now.to_i}"
  FileUtils.move(path, target)
end

def check_dependencies
  `which brew`
  unless $?.success?
    puts "Please install homebrew"
    return false
  end
  unless `brew doctor`.match(/ready to brew/)
    puts "Homebrew is unhappy.  Please run 'brew doctor'"
    return false
  end

  true
end

def install!
  exit(1) unless check_dependencies

  mk_directories
  link_files
  install_core
  configure_defaults

  puts "Note: You need to install fonts manually now."
  puts "Configure complete!"
end

def mk_directories
  DIRECTORIES.each do |path|
    path = File.expand_path("~/#{path}")
    `mkdir -p #{path.shellescape}`
  end
  `mkdir -p backups`
end

def link_files
    LINK_MAP.each do |source, target|
        source = File.expand_path("./#{source}")
	target = File.expand_path("~/#{target}")
        if File.symlink?(target) && File.readlink(target) == source
          next
        elsif File.exists?(target)
          if yes_or_no_prompt("#{target} already exists. Back it up and replace it?")
            backup_file(target)
          else
            puts "Skipping #{source}"
            next
          end
        end
        puts "Linking #{target} to #{source}"
        File.symlink(source, target)
    end
end

def install_core
  puts 'Updating system gems'
  `sudo gem update --system`

  puts 'Installing homebrew packages'
  BREW_PACKAGES.each do |pkg|
    `brew install #{pkg}`
  end
end

def configure_defaults
  `defaults write com.apple.Finder AppleShowAllFiles TRUE`
end

install! if __FILE__ == $0
