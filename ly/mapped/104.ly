\version "2.14.2"
\include "util.ly"
\version "2.14.2"
\header{ tagline = ""}
\paper {
  print-all-headers = ##t
  ragged-right = ##f
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -3)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  ragged-right = ##f
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #104
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key d\major
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}


sopMusic = \relative c' {
  \repeat volta 2 {
    \partial 4. fis8 fis g |
    a4. fis'8 fis e |
    d4( fis,8) a b a |
    a cis, cis e b' a | \break
    
    a d, d fis fis g |
    a4. a8 fis' e |
    d4( fis,) a8 a | \break
    a8[ gis b] a g e |
    
    d4 b'8\rest a( b a) |
    a cis, cis e b' a | \break
    a d, d a' b a |
    a cis, cis e b' a |
    
    a d, d fis fis g | \break
    a4. a8 fis' e |
    d4( fis,) a8 a |
    a8[ gis b] a g e |
    d4. \break
  }
  \pageBreak
}
sopWords = \lyricmode {
  \set stanza = "1. "
  ’Twere vain to tell thee all I feel, __
  Or say for thee I’d die, or say for thee I’d die;
  I find that words will but con -- ceal __
  What my soul __ would wish to sigh.
  \dropLyricsXI
  Ah, __ well -- a -- day! the sweet -- est \raiseLyrics mel -- o -- dy
  Could nev -- er, nev -- er say one half my love for thee,
  Then let me si -- lent -- ly re -- veal __
  What my soul __ would wish to sigh.
}

sopWordsII = \lyricmode {
  \set stanza = "2. "
  Thou’st oft -- en called my voice a bird’s, __
  Whose mu -- sic like a spell, whose mu -- sic like a spell,
  Could change to rap -- ture e’en the words __
  Of our slow __ and sad fare -- well.
}

altoMusic = \relative c' {
  d8 d e |
  fis4. a8 a g |
  fis4( d8) fis g fis |
  e a, a cis e e |
  
  d a a d d e |
  fis4. fis8 a g |
  fis4( d) fis8 fis |
  fis8[ e d] cis cis cis |
  
  d4 s8 fis( g fis) |
  e a, a cis e e |
  d d d fis g fis |
  e a, a cis e e |
  
  d d d d d e |
  fis4. fis8 a g |
  fis4( d) fis8 fis |
  fis8[ e d] cis cis cis |
  d4.
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
}
altoWordsIII = \lyricmode {
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  a8 a a |
  d4. d8 d a |
  a4. d8 d d |
  cis e, e a g g |
  
  fis fis fis a a a |
  d4. d8 d a |
  a2 d8 d |
  d8[ b gis] a a g |
  
  fis4 s8 d'4. |
  cis8 e, e a g g |
  fis fis fis d' d d |
  cis e, e a g g |
  
  fis fis fis a a a |
  d4. d8 d a |
  a2 d8 d |
  d8[ b gis] a a g |
  fis4.
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  d8 d d |
  d4. d8 d d |
  d4. d8 d d |
  a a a a a a |
  
  d d d d d d |
  d4. d8 d d |
  d2 d8 d |
  a4. a8 a a |
  
  d4 d8\rest d4. |
  a8 a a a a a |
  d d d d d d |
  a a a a a a |
  
  d d d d d d |
  d4. d8 d d |
  d2 d8 d |
  a4. a8 a a |
  d4.
}
bassWords = \lyricmode {

}

pianoRH = \relative c' {
  
}
pianoLH = \relative c' {
  
}


\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"’Twere Vain to tell"}}
  poet = \markup\oldStyleNum"J. A. Wade (1800–1875)"
  composer = \markup\italic "Swiss Air"
  tagline = ""
}}


